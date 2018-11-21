{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OpenFEC.QueryTypes where

import           Control.Lens
import           Control.Monad    (join, sequence)
import           Data.Aeson       (FromJSON, ToJSON)
import qualified Data.Aeson       as A
import           Data.Aeson.Lens
import qualified Data.Aeson.Types as A
import           Data.Maybe       (catMaybes, isJust)
import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import qualified Data.Vector      as V
import           GHC.Generics     (Generic)

toTextQueryList :: ToJSON a => [a] -> [Text]
toTextQueryList = catMaybes . fmap (A.parseMaybe A.parseJSON . A.toJSON)

type ApiKey = Text
fecApiKey :: ApiKey
fecApiKey = "jjt0sf4z7FVpSAXoTndlnLKl0sDZUFcf3PQjpXrW"

fecMaxPerPage :: Int
fecMaxPerPage = 100

type ElectionYear = Int

data Office = House | Senate | President
instance ToJSON Office where
  toJSON House     = A.String "H"
  toJSON Senate    = A.String "S"
  toJSON President = A.String "P"

data Party = Democrat | Republican | WorkingFamilies | Conservative | Green | Libertarian deriving (Show, Enum, Bounded)
instance ToJSON Party where
  toJSON Democrat        = A.String "DEM"
  toJSON Republican      = A.String "REP"
  toJSON WorkingFamilies = A.String "WFP"
  toJSON Conservative    = A.String "CON"
  toJSON Green           = A.String "GRE"
  toJSON Libertarian     = A.String "LIB"

instance FromJSON Party where
  parseJSON o = A.withText "Party" f o where
    f t = case t of
      "DEM" -> return Democrat
      "REP" -> return Republican
      "WFP" -> return WorkingFamilies
      "CON" -> return Conservative
      "GRE" -> return Green
      "LIB" -> return Libertarian
      _     -> A.typeMismatch "Party" o


type PageNumber = Int

data PageInfo = PageInfo { pages :: PageNumber, count :: Int, page :: PageNumber, per_page :: Int } deriving (Generic, Show)

instance A.FromJSON PageInfo where
  parseJSON = A.genericParseJSON A.defaultOptions

data Page = Page { pagination :: PageInfo , results :: A.Value } deriving (Generic, Show)

instance A.FromJSON Page where
  parseJSON = A.genericParseJSON A.defaultOptions

data FailureStyle = SkipFailed | NoneIfAnyFailed

getAllPages :: forall m b.Monad m => FailureStyle -> (PageNumber -> m Page) -> (A.Value -> Maybe b) -> m (Maybe (V.Vector b))
getAllPages fs getPage decodeOne = nextPage (return []) 1 where
  finalResult vsM =
    let vM = V.concat <$> vsM
    in case fs of
      NoneIfAnyFailed -> join $ sequence <$> vM
      SkipFailed      -> V.mapMaybe id <$> vM
  nextPage vsM n = do
    page <- getPage n
    let totalPages = pages $ pagination page
        decodedM :: Maybe (V.Vector (Maybe b)) = fmap decodeOne <$> (results page ^? _Array) -- Maybe (Vector (Maybe b))
        vsM' = (++) <$> vsM <*> fmap pure decodedM
    if (n < 3) then nextPage vsM' (n+1) else return (finalResult vsM') -- 3 here for testing!!

