{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module OpenFEC.Beam.Types where


import           Database.Beam             (C)
import qualified Database.Beam             as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Query       as B


import           Control.Monad.Identity    (Identity)
import           Data.Scientific           (Scientific)
import           Data.Text                 (Text)
import qualified Data.Vector               as V
import           GHC.Generics              (Generic)

-- query Utilities
runReturningVector :: (B.MonadBeam syntax be handle m, B.FromBackendRow be x) => syntax -> m (V.Vector x)
runReturningVector = fmap V.fromList . B.runReturningList

-- | Run a 'SqlSelect' in a 'MonadBeam' and get the results as a vector
runSelectReturningVector ::
  (B.IsSql92Syntax cmd, B.MonadBeam cmd be hdl m, B.FromBackendRow be a) =>
  B.SqlSelect (B.Sql92SelectSyntax cmd) a -> m (V.Vector a)
runSelectReturningVector (B.SqlSelect s) = runReturningVector (B.selectCmd s)

type CandidateID = Text
type CommitteeID = Text
type Name = Text
type State = Text
type District = Int
type Amount = Scientific

data Party = Democrat | Republican | Independent | WorkingFamilies | Conservative | Green | Libertarian | Other | Unknown deriving (Show, Read, Enum, Bounded, Eq, Ord, Generic)

data Office = House | Senate | President deriving (Show, Read, Enum, Bounded, Eq, Ord, Generic)

data CandidateT f = Candidate
  {
    _candidate_id       :: C f CandidateID
  , _candidate_name     :: C f Name
  , _candidate_office   :: C f Office
  , _candidate_state    :: C f State
  , _candidate_district :: C f District
  , _candidate_party    :: C f Party
  } deriving (Generic)

type Candidate = CandidateT Identity
type CandidateKey = B.PrimaryKey CandidateT Identity

deriving instance Show Candidate
deriving instance Eq Candidate

instance B.Table CandidateT where
  data PrimaryKey CandidateT f = CandidateKey (C f CandidateID) deriving (Generic)
  primaryKey = CandidateKey . _candidate_id

deriving instance Show CandidateKey
deriving instance Eq CandidateKey

instance B.Beamable CandidateT
instance B.Beamable (B.PrimaryKey CandidateT)

data CommitteeT f = Committee
  {
    _committee_id         :: C f CommitteeID
  , _committee_designaton :: C f (Maybe Text)
  , _committee_name       :: C f (Maybe Text)
  , _committee_type_full  :: C f (Maybe Text)
  , _committee_type       :: C f (Maybe Text) -- we should make this its own type, prolly
  } deriving (Generic)

type Committee = CommitteeT Identity
type CommitteeKey = B.PrimaryKey CommitteeT Identity

deriving instance Show Committee
deriving instance Eq Committee

instance B.Table CommitteeT where
  data PrimaryKey CommitteeT f = CommitteeKey (C f CommitteeID) deriving (Generic)
  primaryKey = CommitteeKey . _committee_id

deriving instance Show CommitteeKey
deriving instance Eq CommitteeKey

instance B.Beamable CommitteeT
instance B.Beamable (B.PrimaryKey CommitteeT)

data Candidate_x_CommitteeT f = Candidate_x_Committee
  {
    _candidate_x_committee_candidate_id :: B.PrimaryKey CandidateT f
  , _candidate_x_committee_committee_id :: B.PrimaryKey CommitteeT f
  } deriving (Generic)

type Candidate_x_Committee = Candidate_x_CommitteeT Identity

deriving instance Show Candidate_x_Committee
deriving instance Eq Candidate_x_Committee

instance B.Table Candidate_x_CommitteeT where
  data PrimaryKey Candidate_x_CommitteeT f = Candidate_x_CommitteeKey (B.PrimaryKey CandidateT f) (B.PrimaryKey CommitteeT f) deriving (Generic)
  primaryKey x = Candidate_x_CommitteeKey (_candidate_x_committee_candidate_id x) (_candidate_x_committee_committee_id x)

type Candidate_x_CommitteeKey = B.PrimaryKey Candidate_x_CommitteeT Identity

instance B.Beamable Candidate_x_CommitteeT
instance B.Beamable (B.PrimaryKey Candidate_x_CommitteeT)


committeesCandidateRelationship :: B.ManyToMany OpenFEC_DB CandidateT CommitteeT
committeesCandidateRelationship = B.manyToMany_ (_openFEC_DB_candidate_x_committee openFEC_DB) _candidate_x_committee_candidate_id _candidate_x_committee_committee_id

data OpenFEC_DB f = OpenFEC_DB
  {
    _openFEC_DB_candidate :: f (B.TableEntity CandidateT)
  , _openFEC_DB_committee :: f (B.TableEntity CommitteeT)
  , _openFEC_DB_candidate_x_committee :: f (B.TableEntity Candidate_x_CommitteeT)
  } deriving (Generic)

instance B.Database be OpenFEC_DB

openFEC_DB :: B.DatabaseSettings be OpenFEC_DB
openFEC_DB = B.defaultDbSettings
