{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where


import qualified OpenFEC.API                              as FEC
import qualified OpenFEC.Types                            as FEC

import           ExploreFEC.Data.Spending                 (CandidateSpending (..),
                                                           describeSpending,
                                                           getCandidateSpending,
                                                           getPresidentialRaceSpending)

import           OpenFEC.Beam.Sqlite.CustomFields         ()
import qualified OpenFEC.Beam.Types                       as FEC


import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as BE
import qualified Database.Beam.Sqlite                     as B

import           Database.Beam.Migrate                    (CheckedDatabaseSettings,
                                                           defaultMigratableDbSettings)
import           Database.Beam.Migrate.Simple             (autoMigrate)
import           Database.Beam.Sqlite.Migrate             (migrationBackend)
import qualified Database.SQLite.Simple                   as SL

import           Control.Applicative                      ((<*>))
import qualified Control.Foldl                            as FL
import           Control.Lens                             (Lens', (.~), (^.))
import           Control.Monad                            (forM_, join, mapM_,
                                                           sequence)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Control.Monad.State                      as S

import           Data.Aeson                               (decodeFileStrict,
                                                           encodeFile)
import qualified Data.Foldable                            as F
import qualified Data.FuzzySet                            as FS
import qualified Data.List                                as L
import qualified Data.Map                                 as M
import           Data.Maybe                               (fromMaybe, isNothing)
import qualified Data.Sequence                            as Seq
import qualified Data.Text                                as T
import           Data.Time.Format                         (defaultTimeLocale,
                                                           parseTimeM)
import           Data.Time.LocalTime                      (LocalTime)
import qualified Data.Vector                              as V
--import           Data.Vinyl                               (ElField (..))
--import           Data.Vinyl.Curry                         (runcurryX)
--import           Data.Vinyl.Lens                          (rlens, rlens')
import           Network.HTTP.Client                      (Manager, defaultManagerSettings,
                                                           managerModifyRequest,
                                                           managerResponseTimeout,
                                                           newManager,
                                                           responseTimeoutMicro)
import           Network.HTTP.Client.TLS                  (tlsManagerSettings)
import           Servant.Client                           (ClientM,
                                                           ServantError,
                                                           mkClientEnv,
                                                           runClientM)

--import           Frames

--import qualified Text.PrettyPrint.Tabulate                as PP



main :: IO ()
main = do
  let electionYear = 2016
      cIds = ["P80001571"]
      payees = []
      jsonFileName = "spending.json"
      queryData = True
      doIf doIt action = if doIt then action else return ()
      managerSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro (120000000)
                                           , managerModifyRequest = \req -> FEC.delayQueries FEC.fecQueryLimit >> {- putStrLn req >> -}  return req
                                           }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager FEC.baseUrl
      runServant :: forall a. ClientM a -> IO (Either ServantError a)
      runServant x = runClientM x clientEnv
  -- find expenditures for a candidate to a certain payee whether they are from campaign committee, independent or party.  And collect the involved committees.
      query = do
        candidates <- FEC.getCandidatesById cIds
        sequence ((\x -> getCandidateSpending x electionYear payees) <$> V.toList candidates)
  doIf queryData $ do
    result <- runClientM query clientEnv
    case result of
      Left err -> putStrLn $ "Query returned an error: " ++ show err
      Right x  -> encodeFile jsonFileName x
  spending :: [CandidateSpending] <- fmap (fromMaybe []) (decodeFileStrict jsonFileName)
  mapM_ (print . describeSpending) spending
  return ()


