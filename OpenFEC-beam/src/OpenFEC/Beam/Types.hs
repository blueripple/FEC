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
{-# LANGUAGE UndecidableInstances  #-}
module OpenFEC.Beam.Types where


import           Database.Beam                    (C)
import qualified Database.Beam                    as B
import qualified Database.Beam.Backend            as B
import qualified Database.Beam.Backend.SQL        as B

import qualified Database.Beam.Sqlite             as B
import qualified Database.SQLite.Simple.FromField as SL

--import qualified Database.Beam.Sqlite   as B

import           Control.Monad.Identity           (Identity)
import           Data.Scientific                  (Scientific)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Text.Read                        (readMaybe)

type CandidateID = Text
type CommitteeID = Text
type Name = Text
type State = Text
type District = Int
type Amount = Scientific

data Party = Democrat | Republican | WorkingFamilies | Conservative | Green | Libertarian | Unknown deriving (Show, Read, Enum, Bounded, Eq, Ord, Generic)

-- marshall to DB as string via show. so VARCHAR 15
instance B.HasSqlValueSyntax be String => B.HasSqlValueSyntax be Party where
  sqlValueSyntax = B.autoSqlValueSyntax

instance SL.FromField Party where
  fromField f = do
    x <- readMaybe <$> SL.fromField f
    case x of
      Nothing -> SL.returnError SL.ConversionFailed f "Could not 'read' value for Party"
      Just x -> pure x

instance B.FromBackendRow B.Sqlite Party


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
