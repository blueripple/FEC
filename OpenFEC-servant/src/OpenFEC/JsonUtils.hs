{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OpenFEC.JsonUtils where

import           Control.Lens         (Getting, (^?))
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

(|#|) :: (A.FromJSON a, A.ToJSON a, Typeable a) => A.Value -> Text -> Either ByteString a
(|#|) = flip tryKey
