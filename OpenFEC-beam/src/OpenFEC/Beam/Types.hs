{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module OpenFEC.Beam.Types where


import           Database.Beam          (C)
import qualified Database.Beam          as B
--import qualified Database.Beam.Sqlite   as B

import           Control.Monad.Identity (Identity)
import           Data.Scientific        (Scientific)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)

type CandidateID = Text
type CommitteeID = Text
type Name = Text
type State = Text
type District = Int
type Amount = Scientific

data Party = Democrat | Republican | WorkingFamilies | Conservative | Green | Libertarian | Unknown deriving (Show, Enum, Bounded, Eq, Ord)

data CandidateT f = CandidateT
  {
    _candidate_id       :: C f CandidateID
  , _candidate_name     :: C f Name
  , _candidate_state    :: C f State
  , _candidate_district :: C f District
  , _candidate_party    :: C f Party
  } deriving (Generic)

type Candidate = CandidateT Identity
type CandidateKey = B.PrimaryKey CandidateT Identity

deriving instance Show Candidate
deriving instance Eq Candidate

instance B.Beamable CandidateT


instance B.Table CandidateT where
  data PrimaryKey CandidateT f = CandidateKey (C f CandidateID) deriving (Generic)
  primaryKey = CandidateKey . _candidate_id

instance B.Beamable (B.PrimaryKey CandidateT)


data OpenFEC_DB f = OpenFEC_DB
  {
    _candidates :: f (B.TableEntity CandidateT)
  } deriving (Generic)

instance B.Database be OpenFEC_DB

openFEC_DB :: B.DatabaseSettings be OpenFEC_DB
openFEC_DB = B.defaultDbSettings
