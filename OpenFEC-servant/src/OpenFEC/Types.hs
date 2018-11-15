{-# LANGUAGE OverloadedStrings #-}
module OpenFEC.Types where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import           Data.Text  (Text, pack, unpack)
{-
import qualified Text.MegaParsec as MP
import qualified Text.MegaParsec.Char as MP
-}
{-
class OpenFECRep a where
  toFECText :: a -> Text
  fromFECText :: Text -> Maybe a

toFECString :: OpenFECRep a => a -> String
toFECString = unpack . toFECText

fromFECString :: OpenFECRep a => String -> Maybe a
fromFECString = fromFECText . pack

instance OpenFECRep Text where
  toFECText = id
  fromFECText = id
-}

type CandidateID = Text
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


