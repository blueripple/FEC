{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
module ByRace where

import           ExploreFEC.Data.Spending                 (CandidateSpending (..),
                                                           describeSpending)

import           OpenFEC.Beam.Sqlite.CustomFields         ()
import qualified OpenFEC.Beam.Types                       as FEC


import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as BE
import qualified Database.Beam.Sqlite                     as B
import qualified Database.SQLite.Simple                   as SL

import           Control.Arrow                            ((&&&))
import qualified Control.Foldl                            as FL
import           Control.Lens                             (to, (.=), (.~), (^.))
import           Control.Monad                            (forM_)
import           Data.Default                             (Default)
import qualified Data.Foldable                            as F
import qualified Data.List                                as L
import qualified Data.List                                as L
import qualified Data.Map                                 as M
import           Data.Maybe                               (fromMaybe)
import qualified Data.Sequence                            as Seq
import qualified Data.Set                                 as S
import qualified Data.Text                                as T
import           Data.Time.Calendar.WeekDate              (toWeekDate)
import           Data.Time.LocalTime                      (LocalTime (..))
import           Data.Tuple.Select                        (sel1, sel2, sel3,
                                                           sel4, sel5, sel6,
                                                           sel7, sel8, sel9)
import           Data.Tuple.Update                        (upd5, upd6, upd7,
                                                           upd8)
import           Formattable.NumFormat                    (formatNum, usdFmt)

import qualified Graphics.Rendering.Chart.Easy            as C


data Forecast = Forecast { winP :: Double, voteShare :: Double {-, voteShare10 :: Double, voteShare90 :: Double -} } deriving (Show)
data Spending = Spending { disbursement :: FEC.Amount, indSupport :: FEC.Amount, indOppose :: FEC.Amount, party :: FEC.Amount } deriving (Show)

allRaces :: SL.Connection -> FEC.Office -> IO [(FEC.State, FEC.District)]
allRaces dbConn office = do
  let allCandidates =  B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
      allHouseCandidates = B.filter_ (\c -> FEC._candidate_office c B.==. B.val_ office) $ allCandidates
      allElections = B.nub_ $ fmap (\c -> (FEC._candidate_state c, FEC._candidate_district c)) $ allHouseCandidates
  B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ allElections


electionResults :: SL.Connection -> IO (M.Map FEC.CandidateID Double)
electionResults dbConn = do
  asList <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ do
    result <- B.all_ (FEC._openFEC_DB_electionResult FEC.openFEC_DB)
    pure (result ^. FEC.electionResult_candidate_id, result ^. FEC.electionResult_voteshare)
  return $ M.fromList asList

spendingAndForecastByRace :: SL.Connection
                          -> FEC.Office
                          -> FEC.State
                          -> Maybe FEC.District
                          -> IO [(FEC.CandidateID, T.Text, LocalTime, Double, Double, FEC.Amount, FEC.Amount, FEC.Amount, FEC.Amount)]
spendingAndForecastByRace dbConn office state districtM = do
  let allCandidates = B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
      allForecasts = B.all_ (FEC._openFEC_DB_forecast538 FEC.openFEC_DB)
      allDisbursements = B.all_ (FEC._openFEC_DB_disbursement FEC.openFEC_DB)
      allIndExpenditures = B.all_ (FEC._openFEC_DB_indExpenditure FEC.openFEC_DB)
      allPartyExpenditures = B.all_ (FEC._openFEC_DB_partyExpenditure FEC.openFEC_DB)
      district = if office /= FEC.House then 0 else fromMaybe 0 districtM -- the 0 default here is bad.  I should throw an error...
      candidatesInElection = B.filter_ (\c -> ((FEC._candidate_state c B.==. B.val_ state)
                                               B.&&. (FEC._candidate_office c B.==. B.val_ office)
                                               B.&&. (FEC._candidate_district c B.==. B.val_ district))) $ allCandidates
      orderedForecasts = B.orderBy_ (\f -> (B.asc_ (FEC._forecast538_candidate_name f), B.asc_ (FEC._forecast538_forecast_date f))) $ allForecasts
      aggregatedDisbursements = B.aggregate_ (\(id, date, amount) -> (B.group_ id, B.group_ date, B.sum_ amount)) $ do
        d <- allDisbursements
        pure (FEC._disbursement_candidate_id d, FEC._disbursement_date d, FEC._disbursement_amount_adj d)
      aggregatedIndExpenditures = B.aggregate_ (\(id, date, supportOpposeFlag, amount) -> (B.group_ id, B.group_ date, B.group_ supportOpposeFlag, B.sum_ amount)) $ do
        ie <- allIndExpenditures
        pure (FEC._indExpenditure_candidate_id ie, FEC._indExpenditure_date ie, FEC._indExpenditure_support_oppose_indicator ie, FEC._indExpenditure_amount ie)
      aggregatedIESupport = B.filter_ (\x -> sel3 x B.==. B.val_ FEC.Support) $  aggregatedIndExpenditures
      aggregatedIEOppose = B.filter_ (\x -> sel3 x B.==. B.val_ FEC.Oppose) $  aggregatedIndExpenditures
      aggregatedPartyExpenditures = B.aggregate_ (\(id, date, amount) -> (B.group_ id, B.group_ date, B.sum_ amount)) $ do
        pe <- allPartyExpenditures
        pure (FEC._partyExpenditure_candidate_id pe, FEC._partyExpenditure_date pe, FEC._partyExpenditure_amount pe)

  forecasts' <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ do
    candidate <- candidatesInElection
    forecast <- orderedForecasts
    disbursement <- B.leftJoin_ aggregatedDisbursements (\(id,date,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    indSupport <- B.leftJoin_ aggregatedIESupport (\(id,date,_,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    indOppose <- B.leftJoin_ aggregatedIEOppose (\(id,date,_,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    partyExpenditures <- B.leftJoin_ aggregatedPartyExpenditures (\(id,date,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    B.guard_ ((FEC._forecast538_candidate_id forecast `B.references_` candidate)
              B.&&. (forecast ^. FEC.forecast538_model B.==. B.val_ "deluxe"))
    pure ( (candidate ^. FEC.candidate_id --forecast ^. FEC.forecast538_candidate_id
         , forecast ^. FEC.forecast538_candidate_name)
         , forecast ^. FEC.forecast538_forecast_date
         , forecast ^. FEC.forecast538_winP
         , forecast ^. FEC.forecast538_voteshare
         , sel3 disbursement
         , sel4 indSupport
         , sel4 indOppose
         , sel3 partyExpenditures)
  let g = maybe 0 (maybe 0 id)
  return $ fmap (\((x0, x1), x2, x3, x4, x5, x6, x7, x8) -> (x0, x1, x2, x3, x4, g x5, g x6, g x7, g x8)) forecasts'


simplifySpendingAndForecast :: [(FEC.CandidateID, T.Text, LocalTime, Double, Double, FEC.Amount, FEC.Amount, FEC.Amount, FEC.Amount)]
                            -> M.Map FEC.CandidateID Double -- election voteshare by Id
                            -> (M.Map T.Text (Maybe Double), [((T.Text, LocalTime), (Double, FEC.Amount))], FEC.Amount)
simplifySpendingAndForecast saf resultById =
  let fixForecast x = (sel1 x, sel2 x, sel3 x, Forecast (sel4 x) (sel5 x), Spending (sel6 x) (sel7 x) (sel8 x) (sel9 x))
      forecasts = fixForecast <$> saf
      addOne x y h Seq.Empty               = Seq.singleton (x,y)
      addOne x y h s@(_ Seq.:|> (_,prevY)) = s Seq.|> (x,h prevY y)
      accum getX getY g = FL.Fold (\seq c -> addOne (getX c) (getY c) g seq) Seq.empty F.toList
      h = accum (\f -> (sel2 f, sel3 f))
      allSpend f = let s = sel5 f in (disbursement s) + (indSupport s) + (indOppose s) + (party s)
      extractMap nameById = F.foldl' (\m (id, name) -> M.insert name (M.lookup id resultById) m) M.empty $ M.toList nameById
      resultByNameMF = FL.Fold (\m f -> M.insert (sel1 f) (sel2 f) m) M.empty extractMap --FL.premap sel1 FL.set
      share = voteShare . sel4
      allSpendNet f = let s = sel5 f in disbursement s + indSupport s + party s - indOppose s
      in FL.fold ((,,)
                  <$> resultByNameMF
                  <*> h (share &&& allSpendNet) (\(_, prevY) (x,y) -> (x,prevY + y))
--                  <*> h (\f -> let s = sel4 f in (disbursement s + indSupport s + party s - indOppose s)) (+)
                  <*> FL.premap allSpend FL.sum) forecasts




topTwoVoteSharesAndTotals :: (M.Map T.Text Double, [((T.Text, LocalTime), (Double, FEC.Amount))], FEC.Amount)
                          -> (T.Text, T.Text, Double, Double, FEC.Amount, FEC.Amount)
topTwoVoteSharesAndTotals (candidatesM, shareAndSpend, total) =
  let topTwo = L.take 2 . reverse $ fst <$> (L.sortBy (\x y -> compare (snd x) (snd y)) $ M.toList candidatesM)
      c1 = head topTwo
      c2 = head $ tail topTwo
      candShareAndSpend x = L.filter (\((c,_),_) -> c == x) shareAndSpend
      css1 = candShareAndSpend c1
      css2 = candShareAndSpend c2
      vsDiffStart = (fst . snd . L.head $ css1) - (fst . snd . L.head $ css2)
      vsDiffEnd = (fst . snd . L.last $ css1) - (fst . snd . L.last $ css2)
      diffSpend = (snd . snd . L.last $ css1) - (snd . snd . L.last $ css2)
  in (c1, c2, vsDiffStart, vsDiffEnd, diffSpend, total)



topTwoDifferentialSpendChart :: FEC.State
                             -> Maybe FEC.District
                             -> (M.Map T.Text Double, [((T.Text, LocalTime), (Double, FEC.Amount))], FEC.Amount)
                             -> C.EC (C.LayoutLR LocalTime Double C.Percent) ()
topTwoDifferentialSpendChart state districtM (candidatesM, shareAndSpend, total) = do
  let topTwo = L.take 2 . reverse $ fst <$> (L.sortBy (\x y -> compare (snd x) (snd y)) $ M.toList candidatesM)
      c1 = head topTwo
      c2 = head $ tail topTwo
      districtTitle = maybe "senate" show districtM
  C.layoutlr_title .= (T.unpack state) ++ "-" ++ districtTitle ++ ": Differential Spending and Forecast Voteshare (" ++ (T.unpack $ formatNum usdFmt total) ++ " spent)"
  C.layoutlr_left_axis . C.laxis_override .= C.axisGridHide
  C.layoutlr_left_axis . C.laxis_title .= "Forecast Vote Share (538)"
  C.layoutlr_right_axis . C.laxis_override .= C.axisGridHide
  C.layoutlr_right_axis . C.laxis_title .= "Spending Differential (% of total spent)"
  forM_ topTwo $ \c -> do
    let vs_points = [fmap (\((_,date),(share,_)) -> (date, share)) $ L.filter (\((cand,_),_) -> cand == c) shareAndSpend]
        vs_style = id
    C.plotLeft $ vs_style <$> (C.line (T.unpack c) $ vs_points)
  let t_points1 = fmap (\((_,date),(_,spend)) -> (date, spend/total)) $ L.filter (\((n,_),_) -> n == c1) shareAndSpend
      t_points2 = fmap (\((_,date),(_,spend)) -> (date, spend/total)) $ L.filter (\((n,_),_) -> n == c2) shareAndSpend
      t_points = [fmap (\((x,y),z) -> (x,C.Percent $ 100*(y-z))) $ zip t_points1 (snd <$> t_points2)]
      d_style = C.plot_lines_style . C.line_dashes .~ [5,5]
  C.plotRight $ d_style <$> (C.line (T.unpack c1 ++ " - " ++ T.unpack c2) $ t_points)

topTwoSpendVsInitialVSDiffScatter :: [(FEC.State, FEC.District, Double, FEC.Amount)] -> C.EC (C.Layout Double Double) ()
topTwoSpendVsInitialVSDiffScatter xs = do
  let points = fmap (\(_,_,x,y) -> (x,y/1000000)) xs
  C.layout_title .= "Total Race Spending vs. August Forecast Voteshare difference (All House Races)"
  C.layout_x_axis . C.laxis_title .= "Difference in % voteshare"
  C.layout_y_axis . C.laxis_title .= "Total Spend (Millions of $)"
  C.plot $ C.points "" points
