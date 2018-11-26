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

import           Control.Exception      (throwIO)
import qualified Data.Aeson             as A
import           Data.Text              (Text)
import           Data.Vector            (Vector, empty)

import qualified OpenFEC.QueryTypes     as FEC
import qualified OpenFEC.Types          as FEC

baseUrl = BaseUrl Https "api.open.fec.gov" 443 "/v1"

data FEC_Routes route = FEC_Routes
  {
    _candidates :: route :- "candidates" :> QueryParam "api_key" Text :> QueryParams "candidate_status" Text :> QueryParams "office" Text :> QueryParams "party" Text :> QueryParams "election_year" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] FEC.Page
  , _committees :: route :- "candidate" :> Capture "candidate_id" Text :> "committees" :> QueryParam "api_key" Text :> QueryParams "cycle" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] FEC.Page
  , _reports :: route :- "committee" :> Capture "committee_id" Text :> "reports" :> QueryParam "api_key" Text :> QueryParams "report_type" Text :> QueryParams "year" Int :> QueryParams "cycle" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] FEC.Page
  }
  deriving (Generic)

fecClients :: FEC_Routes (AsClientT ClientM)
fecClients = genericClient

-- specific useful queries

getCandidatesPage :: [FEC.Office] -> [FEC.Party] -> [FEC.ElectionYear] -> FEC.PageNumber -> ClientM FEC.Page
getCandidatesPage offices parties electionYears page =
  let ttql = FEC.toTextQueryList
  in (_candidates fecClients) (Just FEC.fecApiKey) ["C"] (ttql offices) (ttql parties) electionYears (Just FEC.fecMaxPerPage) (Just page)

getCandidates :: [FEC.Office] -> [FEC.Party] -> [FEC.ElectionYear] -> ClientM (Maybe (Vector FEC.Candidate))
getCandidates offices parties electionYears =
    let getOnePage = getCandidatesPage offices parties electionYears
    in FEC.getAllPages Nothing FEC.SkipFailed getOnePage FEC.candidateFromResultJSON

getCommitteesPage :: FEC.CandidateID -> [FEC.ElectionYear] -> FEC.PageNumber -> ClientM FEC.Page
getCommitteesPage cid years page = (_committees fecClients) cid (Just FEC.fecApiKey) years (Just FEC.fecMaxPerPage) (Just page)

getCommittees :: FEC.CandidateID -> [FEC.ElectionYear] -> ClientM (Maybe (Vector FEC.Committee))
getCommittees cid years =
  let getOnePage = getCommitteesPage cid years
  in FEC.getAllPages Nothing FEC.SkipFailed getOnePage FEC.committeeFromResultJSON

getReportsPage :: FEC.CommitteeID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> FEC.PageNumber -> ClientM FEC.Page
getReportsPage cid reportTypes reportYears electionCycles page =
  (_reports fecClients) cid (Just FEC.fecApiKey) reportTypes reportYears electionCycles (Just FEC.fecMaxPerPage) (Just page)

getReports :: FEC.CommitteeID -> [Text] -> [FEC.ElectionYear] -> [FEC.ElectionCycle] -> ClientM (Maybe (Vector FEC.Report))
getReports cid reportTypes reportYears electionCycles =
  let getOnePage = getReportsPage cid reportTypes reportYears electionCycles
  in FEC.getAllPages Nothing FEC.SkipFailed getOnePage  FEC.reportFromResultJSON
