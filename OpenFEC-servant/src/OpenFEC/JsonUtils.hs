{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OpenFEC.JsonUtils where

import           Control.Lens         (Getting, failing, to, (^?))
import           Control.Monad        (join, sequence)
import qualified Data.Aeson           as A
import           Data.Aeson.Lens      (key, _JSON)
import qualified Data.Aeson.Types     as A
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable        (foldl')
import           Data.Monoid          (First, Monoid, (<>))
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Typeable        (Typeable, typeOf)

tryParse :: Monoid b => (t -> b) -> b -> Getting (First a) t a -> t -> Either b a
tryParse toB msg p x = case (x ^? p) of
  Just a  -> Right a
  Nothing -> Left (msg <> toB x)

tryP :: forall a. Typeable a => ByteString -> Getting (First a) A.Value a -> A.Value -> Either ByteString a
tryP keyName = tryParse A.encode ("JSON parsing failed on \"" <>
                                  keyName
                                  <> "\" as "
                                  <> BS.pack (map (toEnum . fromEnum) $ show $ typeOf (undefined :: a))
                                  <> ". JSON was: ")

tryKeys :: (A.FromJSON a, A.ToJSON a, Typeable a) => [Text] -> A.Value -> Either ByteString a
tryKeys keys val =
  let p = foldl' (.) id (key <$> keys)
      keyName = BS.fromStrict . encodeUtf8 $ foldl' (\k msg -> msg <> "." <> k) "" keys
  in tryP keyName (p . _JSON) val

tryKey :: (A.FromJSON a, A.ToJSON a, Typeable a) => Text -> A.Value -> Either ByteString a
tryKey x = tryKeys [x]

tryTwoKeys :: (Typeable a, A.FromJSON a, A.ToJSON a, A.FromJSON b, A.ToJSON b) => Text -> Text -> (b -> a) -> A.Value -> Either ByteString a
tryTwoKeys key1 key2 f val =
  let k1BS = BS.fromStrict $ encodeUtf8 key1
      k2BS = BS.fromStrict $ encodeUtf8 key2
  in tryP (k1BS <> " AND " <> k2BS) (failing (key key1 . _JSON) (key key2 . _JSON . to f)) val

(|#|) :: (A.FromJSON a, A.ToJSON a, Typeable a) => A.Value -> Text -> Either ByteString a
(|#|) = flip tryKey
