{-# LANGUAGE OverloadedStrings #-}
module OpenFEC.Types where

import           Data.Text (Text,pack,unpack)
import qualified Text.MegaParsec as MP
import qualified Text.MegaParsec.Char as MP

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

type CandidateID = Text
type APIKey = Text

data Office = House | Senate | President
instance OpenFECRep Office where
  toFECText House = "H"
  toFECText Senate = "S"
  toFECText President = "P"
  fromFECText 


data Party = Democrat | Republican | WorkingFamilies | Conservative | Green | Libertarian
