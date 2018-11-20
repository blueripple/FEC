{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module FECExplore.Data.QueryTypes where

import           Control.Monad    (sequence)
import           Data.Aeson       (FromJSON, ToJSON)
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Maybe       (catMaybes)
import           Data.Text        (Text, pack, unpack)
import qualified Data.Text        as T

toTextQueryList :: ToJSON a => [a] -> [T.Text]
toTextQueryList = catMaybes . fmap (A.parseMaybe A.parseJSON . A.toJSON)

--type CandidateID = Text
type APIKey = Text
type ElectionYear = Int

data Office = House | Senate | President
instance ToJSON Office where
  toJSON House     = A.String "H"
  toJSON Senate    = A.String "S"
  toJSON President = A.String "P"

data Party = Democrat | Republican | WorkingFamilies | Conservative | Green | Libertarian
instance ToJSON Party where
  toJSON Democrat        = A.String "DEM"
  toJSON Republican      = A.String "REP"
  toJSON WorkingFamilies = A.String "WFP"
  toJSON Conservative    = A.String "CON"
  toJSON Green           = A.String "GRE"
  toJSON Libertarian     = A.String "LIB"


