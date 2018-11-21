{-# LANGUAGE OverloadedStrings #-}
module OpenFEC.Types where

import           Control.Lens       ((^?))
import qualified Data.Aeson         as A
import           Data.Aeson.Lens
import           Data.Text          (Text)
import qualified OpenFEC.QueryTypes as QT

type CandidateID = Text
type Name = Text
type State = Text
type District = Integer

data Candidate = Candidate
  {
    id       :: CandidateID
  , name     :: Name
  , state    :: State
  , district :: District
  , party    :: QT.Party
  } deriving (Show)

-- we don't really want to and from JSON of this thing so much as from the candidate query result to this

candidateFromResultJSON :: A.Value -> Maybe Candidate
candidateFromResultJSON val =
  let idM = val ^? key "candidate_id" . _String
      nameM = val ^? key "name" . _String
      stateM = val ^? key "state" . _String
      districtM = val ^? key "district_number" . _Integer
      partyM = val ^? key "party" . _JSON -- this is...magic
  in Candidate <$> idM <*> nameM <*> stateM <*> districtM <*> partyM
