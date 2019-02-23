{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
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


import           ExploreFEC.Config                        (DataSources (..))
import           ExploreFEC.Data.Spending                 (CandidateSpending (..),
                                                           describeSpending)

import           OpenFEC.Beam.Sqlite.CustomFields         ()
import qualified OpenFEC.Beam.Types                       as FEC

import           ByRace

import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as BE
import qualified Database.Beam.Sqlite                     as B
import qualified Database.SQLite.Simple                   as SL

import qualified Control.Foldl                            as FL
import           Control.Lens                             (to, (.=), (.~), (^.))
import           Control.Monad                            (forM, forM_)
import qualified Data.Foldable                            as F
import qualified Data.List                                as L
import qualified Data.Map                                 as M
import           Data.Maybe                               (catMaybes)
import qualified Data.Sequence                            as Seq
import qualified Data.Set                                 as S
import qualified Data.Text                                as T
import           Data.Time.Calendar.WeekDate              (toWeekDate)
import           Data.Time.LocalTime                      (LocalTime (..))
import           Data.Tuple.Select                        (sel1, sel2, sel3,
                                                           sel4, sel5, sel6,
                                                           sel7, sel8)
import           Formattable.NumFormat                    (formatNum, usdFmt)
import           System.IO                                (hFlush, stdout)

import qualified Dhall                                    as D

import qualified Graphics.Rendering.Chart.Backend.Cairo   as C
import qualified Graphics.Rendering.Chart.Easy            as C


main :: IO ()
main = do
  dataSources <- D.input D.auto "./config/dataSources.dhall"
  dbConn <- SL.open (openFEC_2018_Db dataSources) --openFEC_SqlitePath
--  forecasts <- spendingAndForecastByRace dbConn FEC.House state (Just district)
--  let simplified = simplifySpendingAndForecast forecasts
  --C.toFile C.def (state ++ "-" ++ show district ++ ".png") $ topTwoDifferentialSpendChart state (Just district) simplified
  allHouseRaces <- fmap L.sort $ allRaces dbConn FEC.House
  x <- forM allHouseRaces $ \(s,d) -> do
    putStr $ (T.unpack s) ++ "-" ++ show d
    results <- electionResults dbConn
    saf <- spendingAndForecastByRace dbConn FEC.House s (Just d)
    let (candM,spend,tot) = simplifySpendingAndForecast saf results
        cm = M.mapMaybe id candM
    res <- case (M.size cm < 2) of
      True -> putStr "x" >> return Nothing
      False -> do
        let (_,_,vsDiffStart,_,_,total) = topTwoVoteSharesAndTotals (cm,spend,tot)
        return $ Just (s, d, vsDiffStart, total)
    putStr ", "
    hFlush stdout
    return res
  putStrLn "\nCreating scatter plot, writing to spendVsVoteshare.png"
  C.toFile C.def ("spendVsVoteshare.png") $ topTwoSpendVsInitialVSDiffScatter (catMaybes x)
