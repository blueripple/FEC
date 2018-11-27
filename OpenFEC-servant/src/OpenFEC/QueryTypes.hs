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
type ElectionCycle = Int

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

data Page = Page { pagination :: PageInfo , results :: V.Vector A.Value } deriving (Generic, Show)

instance A.FromJSON Page where
  parseJSON = A.genericParseJSON A.defaultOptions

data FailureStyle = SkipFailed | NoneIfAnyFailed

getAllPages :: forall m b.
  Monad m =>
  Maybe Int ->
  FailureStyle ->
  (PageNumber -> m Page) ->
  (A.Value -> Maybe b) ->
  m (Maybe (V.Vector b))
getAllPages maxPagesM fs getPage decodeOne = nextPage [] 1 where
  finalResult vs =
    let v = V.concat vs
    in case fs of
      NoneIfAnyFailed -> sequence v
      SkipFailed      -> Just $ V.mapMaybe id v
  nextPage vs n = do
    page <- getPage n
    let totalPages = pages $ pagination page
        decoded :: V.Vector (Maybe b) = decodeOne <$> results page -- Vector (Maybe b)
        vs' = vs ++ [decoded]
    if (n < maybe totalPages id maxPagesM) then nextPage vs' (n+1) else return (finalResult vs') -- 3 here for testing!!



data LastIndex a = LastIndex { lastIndex :: Int, lastOther :: a }

data IndexedPage a = IndexedPage { lastIndexInfo :: LastIndex a, indexedResults :: V.Vector A.Value }

getIndexedPage :: FromJSON a => Text -> A.Value -> Maybe (IndexedPage a)
getIndexedPage k val =
  let lastIndexM = val ^? key "pagination" . key "last_indexes" . key "last_index" . _Integer
      lastOtherM = val ^? key "pagination" . key "last_indexes" . key k . _JSON
      resultsM = val ^? key "results" . _Array
      lastIndexInfoM = LastIndex <$> lastIndexM <*> lastOtherM
  in Index <$> lastIndexInfoM <*> resultsM

getAllIndexedPages :: forall m a b. (Monad m, FromJSON a) =>
                      Maybe Int ->
                      FailureStyle ->
                      (Maybe (LastIndex a) -> m (IndexedPage a)) ->
                      (A.Value -> Maybe b) ->
                      m (Maybe (V.Vector b))
getAllIndexedPages maxPagesM fs getPage decodeOne = nextPage [] Nothing where
  finalResult vs =
    let v = V.concat vs
    in case fs of
      NoneIfAnyFailed -> sequence v
      SkipFailed      -> Just $ V.mapMaybe id v
  nextPage vs lastIndexM = do
    indexedPage <- getPage lastIndexM
    let decoded = decodeOne <$> (indexedResults indexedPage)
        finished = V.null decoded
        vs' = vs ++ [decoded]
    if (not finished) then nextPage vs' (Just $ lastIndex indexedPage) else return (finalResult vs')

