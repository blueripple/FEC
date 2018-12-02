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

data CandidateT f = CandidateT
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

instance B.Beamable CandidateT

instance B.Table CandidateT where
  data PrimaryKey CandidateT f = CandidateKey (C f CandidateID) deriving (Generic)
  primaryKey = CandidateKey . _candidate_id

instance B.Beamable (B.PrimaryKey CandidateT)


data OpenFEC_DB f = OpenFEC_DB
  {
    _openFEC_DB_candidates :: f (B.TableEntity CandidateT)
  } deriving (Generic)

instance B.Database be OpenFEC_DB

openFEC_DB :: B.DatabaseSettings be OpenFEC_DB
openFEC_DB = B.defaultDbSettings
