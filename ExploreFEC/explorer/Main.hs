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
--import           Text.Printf                              (printf)
import           System.IO                                (hFlush, stdout)

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

openFEC_SqlitePath = "/Users/adam/DataScience/DBs/FEC.db"

main :: IO ()
main = do
  dbConn <- SL.open openFEC_SqlitePath
--  forecasts <- spendingAndForecastByRace dbConn FEC.House state (Just district)
--  let simplified = simplifySpendingAndForecast forecasts
  --C.toFile C.def (state ++ "-" ++ show district ++ ".png") $ topTwoDifferentialSpendChart state (Just district) simplified
  allHouseRaces <- fmap L.sort $ allRaces dbConn FEC.House
  x <- forM allHouseRaces $ \(s,d) -> do
    putStr $ (T.unpack s) ++ "-" ++ show d
    saf <- spendingAndForecastByRace dbConn FEC.House s (Just d)
    let ssaf@(candM,_,_) = simplifySpendingAndForecast saf
    res <- case (M.size candM < 2) of
      True -> putStr "x" >> return Nothing
      False -> do
        let (_,_,vsDiffStart,_,_,total) = topTwoVoteSharesAndTotals ssaf
        return $ Just (s, d, vsDiffStart, total)
    putStr ", "
    hFlush stdout
    return res
  C.toFile C.def ("spendVsVoteshare.png") $ topTwoSpendVsInitialVSDiffScatter (catMaybes x)
