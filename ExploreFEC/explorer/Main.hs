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
module Main where


import           ExploreFEC.Data.Spending                 (CandidateSpending (..),
                                                           describeSpending)

import           OpenFEC.Beam.Sqlite.CustomFields         ()
import qualified OpenFEC.Beam.Types                       as FEC


import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as BE
import qualified Database.Beam.Sqlite                     as B
import qualified Database.SQLite.Simple                   as SL

import qualified Control.Foldl                            as FL
import           Control.Lens                             (to, (.=), (.~), (^.))
import           Control.Monad                            (forM_)
import qualified Data.Foldable                            as F
import qualified Data.List                                as L
import qualified Data.List                                as L
import qualified Data.Map                                 as M
import qualified Data.Sequence                            as Seq
import qualified Data.Set                                 as S
import qualified Data.Text                                as T
import           Data.Time.Calendar.WeekDate              (toWeekDate)
import           Data.Time.LocalTime                      (LocalTime (..))
import           Data.Tuple.Select                        (sel1, sel2, sel3,
                                                           sel4, sel5, sel6,
                                                           sel7, sel8)
import           Formattable.NumFormat                    (formatNum, usdFmt)
--import           Text.Printf                              (printf)


import qualified Graphics.Rendering.Chart.Backend.Cairo   as C
import qualified Graphics.Rendering.Chart.Easy            as C


{-
import           Control.Applicative                      ((<*>))


import           Control.Monad                            (forM_, join, mapM_,
                                                           sequence)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Control.Monad.State                      as S

import           Data.Aeson                               (encodeFile)


import           Data.Maybe                               (isNothing)



import qualified Data.Vector                              as V

import qualified Text.PrettyPrint.Tabulate                as PP
-}

-- 1st just try to produce csv for one election, aggregated by a given timeframe.

openFEC_SqliteFile = "/Users/adam/DataScience/DBs/FEC.db"

data Forecast = Forecast { winP :: Double, voteShare :: Double {-, voteShare10 :: Double, voteShare90 :: Double -} } deriving (Show)
data Spending = Spending { disbursement :: FEC.Amount, indSupport :: FEC.Amount, indOppose :: FEC.Amount, party :: FEC.Amount } deriving (Show)

main :: IO ()
main = do
  let state = "GA"
      district = 6
      allCandidates = B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
      allForecasts = B.all_ (FEC._openFEC_DB_forecast538 FEC.openFEC_DB)
      allDisbursements = B.all_ (FEC._openFEC_DB_disbursement FEC.openFEC_DB)
      allIndExpenditures = B.all_ (FEC._openFEC_DB_indExpenditure FEC.openFEC_DB)
      allPartyExpenditures = B.all_ (FEC._openFEC_DB_partyExpenditure FEC.openFEC_DB)
      getDay (LocalTime day _) = day
      getWeekDay (_,_,wd) = wd
      candidatesInElection = B.filter_ (\c -> ((FEC._candidate_state c B.==. B.val_ state) B.&&. (FEC._candidate_district c B.==. B.val_ district))) $ allCandidates
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
  dbConn <- SL.open openFEC_SqliteFile
  forecasts' <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ do
    candidate <- candidatesInElection
    forecast <- orderedForecasts
    disbursement <- B.leftJoin_ aggregatedDisbursements (\(id,date,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    indSupport <- B.leftJoin_ aggregatedIESupport (\(id,date,_,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    indOppose <- B.leftJoin_ aggregatedIEOppose (\(id,date,_,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    partyExpenditures <- B.leftJoin_ aggregatedPartyExpenditures (\(id,date,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    B.guard_ ((FEC._forecast538_candidate_id forecast `B.references_` candidate)
              B.&&. (forecast ^. FEC.forecast538_model B.==. B.val_ "deluxe"))
    pure ( forecast ^. FEC.forecast538_candidate_name
         , forecast ^. FEC.forecast538_forecast_date
         , forecast ^. FEC.forecast538_winP
         , forecast ^. FEC.forecast538_voteshare
         , sel3 disbursement
         , sel4 indSupport
         , sel4 indOppose
         , sel3 partyExpenditures)
  let g = maybe 0 (maybe 0 id)
      fixForecast x = (sel1 x, sel2 x, Forecast (sel3 x) (sel4 x), Spending (g (sel5 x)) (g (sel6 x)) (g (sel7 x)) (g (sel8 x)))
      forecasts = fixForecast <$> forecasts'
--      addOne :: Num b => a -> b -> Seq.Seq (a,b) -> Seq.Seq (a,b)
      addOne x y h Seq.Empty               = Seq.singleton (x,y)
      addOne x y h s@(_ Seq.:|> (_,prevY)) = s Seq.|> (x,h prevY y)
  --    accum :: (c -> a) -> (c -> b) -> FL.Fold c [(a,b)]
      accum getX getY g = FL.Fold (\seq c -> addOne (getX c) (getY c) g seq) Seq.empty F.toList
      h = accum (\f -> (sel1 f, sel2 f))
      allSpend f = let s = sel4 f in (disbursement s) + (indSupport s) + (indOppose s) + (party s)
      candidatesMF = FL.Fold (\m f -> M.insert (sel1 f) (voteShare . sel3 $ f) m) M.empty id --FL.premap sel1 FL.set
      (candidatesM, vs, ts, total) =
        let spend = sel4
        in FL.fold ((,,,)
                     <$> candidatesMF
                     <*> h (voteShare . sel3) (\x y -> y)
                     <*> h (\f -> let s = sel4 f in (disbursement s + indSupport s + party s - indOppose s)) (+)
                     <*> FL.premap allSpend FL.sum) forecasts
      topTwo = L.take 2 . reverse $ fst <$> (L.sortBy (\x y -> compare (snd x) (snd y)) $ M.toList candidatesM)
      c1 = head topTwo
      c2 = head $ tail topTwo
  C.toFile C.def (state ++ "-" ++ show district ++ ".png") $ do
    C.layoutlr_title .= "Differential Spending and Forecast Voteshare (" ++ (T.unpack $ formatNum usdFmt total) ++ " spent)"
    C.layoutlr_left_axis . C.laxis_override .= C.axisGridHide
    C.layoutlr_left_axis . C.laxis_title .= "Forecast Vote Share (538)"
    C.layoutlr_right_axis . C.laxis_override .= C.axisGridHide
    C.layoutlr_right_axis . C.laxis_title .= "Spending Differential (% of total spent)"
    forM_ topTwo $ \c -> do
      let vs_points = [fmap (\((_,x),y) -> (x, y)) $ L.filter (\((n,_),_) -> n == c) vs]
          vs_style = id
      C.plotLeft $ vs_style <$> (C.line (T.unpack c) $ vs_points)
    let t_points1 = fmap (\((_,x),y) -> (x, y/total)) $ L.filter (\((n,_),_) -> n == c1) ts
        t_points2 = fmap (\((_,x),y) -> (x, y/total)) $ L.filter (\((n,_),_) -> n == c2) ts
        t_points = [fmap (\((x,y),z) -> (x,C.Percent $ 100*(y-z))) $ zip t_points1 (snd <$> t_points2)]
        d_style = C.plot_lines_style . C.line_dashes .~ [5,5]
    C.plotRight $ d_style <$> (C.line (T.unpack c1 ++ " - " ++ T.unpack c2) $ t_points)

