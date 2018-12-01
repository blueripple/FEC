{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OpenFEC.Pagination where

import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Lens
import           Control.Monad          (join, sequence)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Aeson             as A
import           Data.Aeson.Lens
import qualified Data.Aeson.Types       as A
import           Data.ByteString.Lazy   (ByteString)
import           Data.Either            (fromRight)
import           Data.Maybe             (catMaybes, isJust)
import           Data.Maybe             (isNothing)
import           Data.Monoid            ((<>))
import           Data.Text              (Text, unpack)
import           Data.Typeable          (Typeable)
import qualified Data.Vector            as V
import           GHC.Generics           (Generic)
import           Text.Read              (readMaybe)

import           Servant

import           OpenFEC.JsonUtils


type PageNumber = Int

data PageInfo = PageInfo { pages :: PageNumber, count :: Int, page :: PageNumber, per_page :: Int } deriving (Generic, Show)

instance A.FromJSON PageInfo where
  parseJSON = A.genericParseJSON A.defaultOptions

data Page = Page { pagination :: PageInfo , results :: V.Vector A.Value } deriving (Generic, Show)

instance A.FromJSON Page where
  parseJSON = A.genericParseJSON A.defaultOptions

data FailureStyle = SkipFailed | NoneIfAnyFailed

eitherParse :: (A.Value -> Maybe b) -> A.Value -> Either A.Value b
eitherParse f json = maybe (Left json) Right (f json)

getAllPages :: forall m b.
  (Monad m, MonadThrow m) =>
  Maybe Int ->
  FailureStyle ->
  (PageNumber -> m Page) ->
  (A.Value -> Either ByteString b) ->
  m (V.Vector b)
getAllPages maxPagesM fs getPage decodeOne = nextPage [] 1 where
  finalResult vs =
    let v = V.concat vs
    in case fs of
      NoneIfAnyFailed -> case (sequence v) of
        Left errBS -> throw $ err417 { errBody = "Failure decoding one or more items in getAllPages.\n" <>  errBS}
        Right parsed -> return parsed
      SkipFailed      -> return $ V.mapMaybe (either (const Nothing) Just) v
  nextPage vs n = do
    page <- getPage n
    let totalPages = pages $ pagination page
        decoded :: V.Vector (Either ByteString b) = decodeOne <$> results page -- Vector (Either A.Value Maybe b)
        vs' = vs ++ [decoded]
    if (n < maybe totalPages id maxPagesM) then nextPage vs' (n+1) else finalResult vs'

data LastIndex a = LastIndex { lastIndex :: Int, lastOther :: a }

data IndexedPage a = IndexedPage { lastIndexInfo :: Maybe (LastIndex a), resultsPerPage :: Int, indexedResults :: V.Vector A.Value }

getIndexedPageE :: (Typeable a, FromJSON a, ToJSON a) => Text -> A.Value -> Either ByteString (IndexedPage a)
getIndexedPageE k val =
  let postConversion = either Left (maybe (Left "Error converting lastIndex Text to Int") Right)
      lastIndexJsonM = val ^? key "pagination" . key "last_indexes" . nonNull
      lastIndexME = case lastIndexJsonM of
        Nothing -> Right Nothing
        Just j ->  Just <$> (LastIndex
                             <$> (postConversion (readMaybe . unpack <$> j |#| "last_index"))
                             <*> j |#| k)
  in IndexedPage
  <$> lastIndexME
  <*> tryKeys ["pagination","per_page"] val
  <*> val |#| "results"


getAllIndexedPages :: forall m a b. (Monad m, MonadThrow m, FromJSON a, ToJSON a) =>
                      Maybe Int ->
                      FailureStyle ->
                      (Maybe (LastIndex a) -> m (IndexedPage a)) ->
                      (A.Value -> Either ByteString b) ->
                      m (V.Vector b)
getAllIndexedPages maxPagesM fs getPage decodeOne = nextPage [] Nothing 1 where
  finalResult vs =
    let v = V.concat vs
    in case fs of
      NoneIfAnyFailed -> case (sequence v) of
        Left errBS -> throw $ err417 { errBody = "Failure decoding one or more items in getAllIndexedPages.\n" <> errBS }
        Right parsed -> return parsed
      SkipFailed      -> return $ V.mapMaybe (either (const Nothing) Just) v
  nextPage vs lastIndexM n = do
    indexedPage <- getPage lastIndexM
    let indexInfoM = lastIndexInfo indexedPage
    let decoded = decodeOne <$> (indexedResults indexedPage)
        finished = (isNothing indexInfoM) || (V.length decoded < resultsPerPage indexedPage) || (maybe False (n >=) maxPagesM)
        vs' = decoded : vs
    if (not finished) then nextPage vs' indexInfoM (n + 1) else finalResult (reverse vs')

