{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module OpenFEC.API where


import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

import           Control.Exception.Safe (throw)
import qualified Data.Aeson             as A
import           Data.ByteString.Char8  (pack)
import           Data.Monoid            ((<>))
import           Data.Scientific        (Scientific)
import           Data.Text              (Text)
import           Data.Time.LocalTime    (LocalTime)
import           Data.Vector            (Vector, empty)

import qualified OpenFEC.QueryTypes     as FEC
import qualified OpenFEC.Types          as FEC

baseUrl = BaseUrl Https "api.open.fec.gov" 443 "/v1"

data FEC_Routes route = FEC_Routes
  {
    _candidates :: route :- "candidates" :> QueryParam "api_key" Text :> QueryParams "candidate_status" Text :> QueryParams "office" Text :> QueryParams "party" Text :> QueryParams "election_year" Int :> QueryParam "state" Text :> QueryParam "district" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] FEC.Page
  , _committees :: route :- "candidate" :> Capture "candidate_id" Text :> "committees" :> QueryParam "api_key" Text :> QueryParams "cycle" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] FEC.Page
  , _reports :: route :- "committee" :> Capture "committee_id" Text :> "reports" :> QueryParam "api_key" Text :> QueryParams "report_type" Text :> QueryParams "year" Int :> QueryParams "cycle" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] FEC.Page
  , _disbursements :: route :- "schedules" :> "schedule_b" :> QueryParam "api_key" Text :> QueryParam "committee_id" Text :> QueryParam "two_year_transaction_period" Int :> QueryParam "per_page" Int :> QueryParam "last_index" Int :> QueryParam "last_disbursement_date" LocalTime :> Get '[JSON] A.Value
  , _independent_expenditures :: route :- "schedules" :> "schedule_e" :> QueryParam "api_key" Text :> QueryParam "candidate_id" Text :> QueryParam "committee_id" Text :> QueryParams "cycle" Int :> QueryParam "last_index" Int :> QueryParam "last_expenditure_date" LocalTime :> Get '[JSON] A.Value
  , _party_expenditures :: route :- "schedules" :> "schedule_f" :>  QueryParam "api_key" Text :> QueryParam "candidate_id" Text :> QueryParams "cycle" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] FEC.Page
  }
  deriving (Generic)

fecClients :: FEC_Routes (AsClientT ClientM)
fecClients = genericClient

-- specific useful queries

getCandidatesPage :: [FEC.Office] -> [FEC.Party] -> [FEC.ElectionYear] -> Maybe FEC.State -> Maybe FEC.District -> FEC.PageNumber -> ClientM FEC.Page
getCandidatesPage offices parties electionYears stateM districtM page =
  let ttql = FEC.toTextQueryList
  in (_candidates fecClients) (Just FEC.fecApiKey) ["C"] (ttql offices) (ttql parties) electionYears stateM districtM (Just FEC.fecMaxPerPage) (Just page)

getCandidates :: [FEC.Office] -> [FEC.Party] -> [FEC.ElectionYear] ->  Maybe FEC.State -> Maybe FEC.District -> ClientM (Vector FEC.Candidate)
getCandidates offices parties electionYears stateM districtM =
    let getOnePage = getCandidatesPage offices parties electionYears stateM districtM
    in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.candidateFromResultJSON

getHouseCandidates :: FEC.State -> FEC.District -> FEC.ElectionYear -> ClientM (Vector FEC.Candidate)
getHouseCandidates state district electionYear = getCandidates [FEC.House] [] [electionYear] (Just state) (Just district)

getSenateCandidates :: FEC.State -> FEC.ElectionYear -> ClientM (Vector FEC.Candidate)
getSenateCandidates state electionYear = getCandidates [FEC.Senate] [] [electionYear] (Just state) Nothing

getPresidentialCandidates :: FEC.ElectionYear -> ClientM (Vector FEC.Candidate)
getPresidentialCandidates electionYear = getCandidates [FEC.President] [] [electionYear] Nothing Nothing

getCommitteesPage :: FEC.CandidateID -> [FEC.ElectionYear] -> FEC.PageNumber -> ClientM FEC.Page
getCommitteesPage cid years page = (_committees fecClients) cid (Just FEC.fecApiKey) years (Just FEC.fecMaxPerPage) (Just page)

getCommittees :: FEC.CandidateID -> [FEC.ElectionYear] -> ClientM (Vector FEC.Committee)
getCommittees cid years =
  let getOnePage = getCommitteesPage cid years
  in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.committeeFromResultJSON

getReportsPage :: FEC.CommitteeID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> FEC.PageNumber -> ClientM FEC.Page
getReportsPage cid reportTypes reportYears electionCycles page =
  (_reports fecClients) cid (Just FEC.fecApiKey) reportTypes reportYears electionCycles (Just FEC.fecMaxPerPage) (Just page)

getReports :: FEC.CommitteeID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> ClientM (Vector FEC.Report)
getReports cid reportTypes reportYears electionCycles =
  let getOnePage = getReportsPage cid reportTypes reportYears electionCycles
  in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage  FEC.reportFromResultJSON

{-
getReportsByCandidate :: FEC.CandidateID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> ClientM (Maybe (Vector FEC.Report))
getReportsByCandidate id reportType electionYears electionCycles = do
  let getFromCommittee (Committee id _ _ _ _) = getReports id reportTypes reportYears electionCycles
  committeesM <- getCommittees id electionYears
-}

getDisbursementsIPage :: FEC.CommitteeID -> FEC.ElectionYear -> Maybe Int -> Maybe LocalTime -> ClientM (FEC.IndexedPage LocalTime)
getDisbursementsIPage cid electionYear lastIndexM lastDisbursementDateM = do
  json <- (_disbursements fecClients) (Just FEC.fecApiKey) (Just cid) (Just electionYear) (Just FEC.fecMaxPerPage) lastIndexM lastDisbursementDateM
  let parsedE = FEC.getIndexedPageE "last_disbursement_date" json
  case parsedE of
    Left errBS -> throw $ err417 { errBody = "Decoding Error (Aeson.Value -> FEC.IndexedPage LocalTime) in getDisbursementsIPage. " <> errBS }
    Right ip -> return ip

getDisbursements :: FEC.CommitteeID -> FEC.ElectionYear -> ClientM (Vector FEC.Disbursement)
getDisbursements cid electionYear =
  let getOnePage x = case x of
        Nothing -> getDisbursementsIPage cid electionYear Nothing Nothing
        Just (FEC.LastIndex li ldd) -> getDisbursementsIPage cid electionYear (Just li) (Just ldd)
  in FEC.getAllIndexedPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.disbursementFromResultJSON


getIndependentExpendituresByCandidateIPage :: FEC.CandidateID -> [FEC.ElectionYear] -> Maybe Int -> Maybe LocalTime -> ClientM (FEC.IndexedPage LocalTime)
getIndependentExpendituresByCandidateIPage cid cycles liM leM = do
  json <- (_independent_expenditures fecClients) (Just FEC.fecApiKey) (Just cid) Nothing cycles liM leM
  let parsedE = FEC.getIndexedPageE "last_expenditure_date" json
  case parsedE of
    Left errBS -> throw $ err417 { errBody = "Decoding Error (Aeson.Value -> FEC.IndexedPage LocalTime) in getIndependentExpendituresIPageByCandidate. " <> errBS }
    Right ip -> return ip

getIndependentExpendituresByCommitteeIPage :: FEC.CommitteeID -> [FEC.ElectionYear] -> Maybe Int -> Maybe LocalTime -> ClientM (FEC.IndexedPage LocalTime)
getIndependentExpendituresByCommitteeIPage cid cycles liM leM = do
  json <- (_independent_expenditures fecClients) (Just FEC.fecApiKey) Nothing (Just cid) cycles liM leM
  let parsedE = FEC.getIndexedPageE "last_expenditure_date" json
  case parsedE of
    Left errBS -> throw $ err417 { errBody = "Decoding Error (Aeson.Value -> FEC.IndexedPage LocalTime) in getIndependentExpendituresIPageByCandidate. " <> errBS }
    Right ip -> return ip

getIndependentExpendituresByCandidate :: FEC.CandidateID -> [FEC.ElectionYear] -> ClientM (Vector FEC.IndExpenditure)
getIndependentExpendituresByCandidate cid cycles =
  let getOnePage x = case x of
        Nothing -> getIndependentExpendituresByCandidateIPage cid cycles Nothing Nothing
        Just (FEC.LastIndex li led) -> getIndependentExpendituresByCandidateIPage cid cycles (Just li) (Just led)
  in FEC.getAllIndexedPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.indExpenditureFromResultJSON

getIndependentExpendituresByCommittee :: FEC.CommitteeID -> [FEC.ElectionYear] -> ClientM (Vector FEC.IndExpenditure)
getIndependentExpendituresByCommittee cid cycles =
  let getOnePage x = case x of
        Nothing -> getIndependentExpendituresByCommitteeIPage cid cycles Nothing Nothing
        Just (FEC.LastIndex li led) -> getIndependentExpendituresByCommitteeIPage cid cycles (Just li) (Just led)
  in FEC.getAllIndexedPages Nothing FEC.SkipFailed getOnePage FEC.indExpenditureFromResultJSON

getPartyExpendituresPage :: FEC.CandidateID -> [FEC.ElectionYear] -> FEC.PageNumber -> ClientM FEC.Page
getPartyExpendituresPage cid cycles page =
  (_party_expenditures fecClients) (Just FEC.fecApiKey) (Just cid) cycles (Just FEC.fecMaxPerPage) (Just page)

getPartyExpenditures :: FEC.CandidateID -> [FEC.ElectionYear] -> ClientM (Vector FEC.PartyExpenditure)
getPartyExpenditures cid cycles =
  let getOnePage = getPartyExpendituresPage cid cycles
  in FEC.getAllPages Nothing FEC.NoneIfAnyFailed getOnePage FEC.partyExpenditureFromResultJSON

