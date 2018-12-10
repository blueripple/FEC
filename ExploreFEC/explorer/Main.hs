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

import           Control.Lens                             ((^.))
import qualified Data.Text                                as T
import           Data.Time.Calendar.WeekDate              (toWeekDate)
import           Data.Time.LocalTime                      (LocalTime (..))
import           Data.Tuple.Select                        (sel1, sel2, sel3,
                                                           sel4)
{-
import           Control.Applicative                      ((<*>))
import qualified Control.Foldl                            as FL

import           Control.Monad                            (forM_, join, mapM_,
                                                           sequence)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Control.Monad.State                      as S

import           Data.Aeson                               (encodeFile)
import qualified Data.Foldable                            as F
import qualified Data.List                                as L
import qualified Data.Map                                 as M
import           Data.Maybe                               (isNothing)
import qualified Data.Sequence                            as Seq


import qualified Data.Vector                              as V

import qualified Text.PrettyPrint.Tabulate                as PP
-}

-- 1st just try to produce csv for one election, aggregated by a given timeframe.

openFEC_SqliteFile = "/Users/adam/DataScience/DBs/FEC.db"

main :: IO ()
main = do
  let state = "NY"
      district = 11
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
  forecasts <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ do
    candidate <- candidatesInElection
    forecast <- orderedForecasts
    disbursement <- B.leftJoin_ aggregatedDisbursements (\(id,date,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    indSupport <- B.leftJoin_ aggregatedIESupport (\(id,date,_,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    indOppose <- B.leftJoin_ aggregatedIEOppose (\(id,date,_,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    partyExpenditures <- B.leftJoin_ aggregatedPartyExpenditures (\(id,date,_) -> (id `B.references_` candidate) B.&&. (date B.==. forecast ^. FEC.forecast538_forecast_date))
    B.guard_ ((FEC._forecast538_candidate_id forecast `B.references_` candidate)
              B.&&. (forecast ^. FEC.forecast538_model B.==. B.val_ "classic"))
    pure (forecast ^. FEC.forecast538_candidate_name, forecast ^. FEC.forecast538_forecast_date, forecast ^. FEC.forecast538_winP, forecast ^. FEC.forecast538_voteshare, sel3 disbursement, sel4 indSupport, sel4 indOppose, sel3 partyExpenditures)
  print forecasts

