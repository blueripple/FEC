{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExploreFEC.Data.Spending where

import           Servant.Client         (ClientM)

import qualified Control.Lens           as L
import qualified Control.Lens.Fold      as L
import           Control.Monad          (sequence)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson             as A
import           Data.Monoid            (Sum (..))
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import qualified OpenFEC.API            as FEC
import qualified OpenFEC.Types          as FEC


data CandidateSpending = CandidateSpending
  {
    _candidate               :: FEC.Candidate
  , _disbursements           :: V.Vector (FEC.Disbursement)
  , _independentExpenditures :: V.Vector (FEC.IndExpenditure)
  , _partyExpenditures       :: V.Vector (FEC.PartyExpenditure)
  } deriving (Generic)

instance A.FromJSON CandidateSpending where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON CandidateSpending where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

getHouseRaceSpending :: FEC.State -> FEC.District -> FEC.ElectionYear -> ClientM [CandidateSpending]
getHouseRaceSpending state district electionYear = do
  candidates <- FEC.getHouseCandidates state district (Just electionYear) []
  spending <- sequence (flip getCandidateSpending electionYear <$> V.toList candidates)
  liftIO $ sequence $ fmap (putStrLn . T.unpack . describeSpending) spending
  return spending

getSenateRaceSpending :: FEC.State -> FEC.ElectionYear -> ClientM [CandidateSpending]
getSenateRaceSpending state electionYear = do
  candidates <- FEC.getSenateCandidates state (Just electionYear) []
  spending <- sequence (flip getCandidateSpending electionYear <$> V.toList candidates)
  liftIO $ sequence $ fmap (putStrLn . T.unpack . describeSpending) spending
  return spending

-- This might need some tweaking to get 4-year data
getPresidentialRaceSpending :: FEC.ElectionYear -> ClientM [CandidateSpending]
getPresidentialRaceSpending electionYear = do
  candidates <- FEC.getPresidentialCandidates electionYear
  spending <- sequence (flip getCandidateSpending electionYear <$> V.toList candidates)
  liftIO $ sequence $ fmap (putStrLn . T.unpack . describeSpending) spending
  return spending

getCandidateSpending :: FEC.Candidate -> FEC.ElectionYear -> ClientM CandidateSpending
getCandidateSpending cand electionYear = do
  let candidate_id = FEC._candidate_id cand
      getDisbursements committee = FEC.getDisbursements (FEC._committee_id committee) electionYear
  committees <- FEC.getCommitteesByCandidate candidate_id (Just electionYear) []
--  liftIO $ putStrLn $ "committees: " ++ show (FEC._committee_id <$> committees)
  disbursements <- fmap V.concat . sequence $ (getDisbursements <$> V.toList committees)
  indExpenditures <- FEC.getIndependentExpendituresByCandidate candidate_id [electionYear]
  partyExpenditures <- FEC.getPartyExpenditures candidate_id [electionYear]
  return $ CandidateSpending cand disbursements indExpenditures partyExpenditures

describeSpending :: CandidateSpending -> T.Text
describeSpending (CandidateSpending c d i p) =
  let f = T.pack . show
      g = FEC.amountToText
      sum l x = getSum $ L.foldMapOf (L.folded . l) Sum x
      totalD = sum FEC.disbursement_amount d
      totalP = sum FEC.partyExpenditure_amount p
      (iS,iO) = V.partition (\x -> FEC._indExpenditure_support_oppose_indicator x == FEC.Support) i
      totalI_support = sum FEC.indExpenditure_amount_from_ytd iS
      totalI_oppose = sum FEC.indExpenditure_amount_from_ytd iO
      total = totalD + totalP + totalI_support
  in "For " <> (FEC._candidate_name c) <> " (FEC ID: " <> (FEC._candidate_id c)  <> ") we have "
     <> f (V.length d) <> " disbursements ($ " <> g totalD <> ") and "
     <> f (V.length i) <> " independent expenditures ($" <> g totalI_support <> " in support and $" <> g totalI_oppose <> " in opposition) and "
     <> f (V.length p) <> " party expenditures ($ " <> g totalP <> "). Total: $" <> g total <> " in support and $" <> g totalI_oppose <> " in opposition."
