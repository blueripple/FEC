{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
module Main where


import qualified OpenFEC.API                      as FEC
import qualified OpenFEC.Types                    as FEC

import           ExploreFEC.Data.Spending         (getHouseRaceSpending,
                                                   getPresidentialRaceSpending,
                                                   getSenateRaceSpending)

import           OpenFEC.Beam.Sqlite.CustomFields ()
import qualified OpenFEC.Beam.Types               as FEC


import qualified Database.Beam                    as B
import qualified Database.Beam.Sqlite             as B

import           Database.Beam.Migrate            (CheckedDatabaseSettings,
                                                   defaultMigratableDbSettings)
import           Database.Beam.Migrate.Simple     (autoMigrate)
import           Database.Beam.Sqlite.Migrate     (migrationBackend)
import qualified Database.SQLite.Simple           as SL

import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Servant.Client                   (ClientM, ServantError,
                                                   mkClientEnv, runClientM)

import           Control.Monad                    (mapM_, sequence)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       (encodeFile)
import qualified Data.Foldable                    as F
import           Data.List.Split                  (chunksOf)
import           Data.List.Unique                 (repeated)
import qualified Data.Map                         as M
import           Data.Maybe                       (isNothing)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Network.HTTP.Client              (Manager,
                                                   defaultManagerSettings,
                                                   managerModifyRequest,
                                                   newManager)

import qualified Text.PrettyPrint.Tabulate        as PP


-- create the migration
openFEC_DbMigratable :: CheckedDatabaseSettings B.Sqlite FEC.OpenFEC_DB
openFEC_DbMigratable = defaultMigratableDbSettings @B.SqliteCommandSyntax

loadCandidates :: (forall a. ClientM a -> IO (Either ServantError a)) -> SL.Connection -> ([FEC.ElectionYear] -> ClientM (V.Vector FEC.Candidate)) -> [FEC.ElectionYear] -> IO ()
loadCandidates runServant dbConn getCands electionYears = do
  servantRes <- runServant $ getCands electionYears
  case servantRes of
    Left err -> putStrLn $ "Servant Query returned an error: " ++ show err
    Right candidatesV -> do
      let candidatesL = V.toList candidatesV
          unique_candidatesL = snd $ F.foldl' (\(seen, newList) c ->
                                                   let cid = (FEC._candidate_id c)
                                                       already = isNothing (M.lookup cid seen)
                                                   in (M.insert cid 0 seen, if already then newList else c : newList)) (M.empty, []) candidatesL

          dups = repeated $ (FEC._candidate_id <$> unique_candidatesL)
      putStrLn $ "dups: " ++ show dups
      B.runBeamSqlite dbConn $ do
        autoMigrate migrationBackend openFEC_DbMigratable
        mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_candidates FEC.openFEC_DB) . B.insertValues) $ chunksOf 100 unique_candidatesL
        return ()

openFEC_SqliteFile = "../DBs/FEC-test.db"

main :: IO ()
main = do
  let raceTypes = [FEC.House]
      parties = [FEC.Democrat, FEC.Republican, FEC.Green]
      electionYears = [2018]
--      query = FEC.getCandidates raceType parties electionYears
--      query = FEC.getFilings "H8NY11113" True [2018]
--      query = FEC.getCommittees "H8NY11113" []
--      query = FEC.getReports "C00652248" [] [] []
--      query = FEC.getDisbursements "C00652248" 2018
--      query = FEC.getIndependentExpendituresByCandidate "H6NY11174" [2018]
--      query = FEC.getPartyExpenditures "H0CA27085" []
--      query = getHouseRaceSpending "NY" 11 2018
--      query = getSenateRaceSpending "FL" 2018
      managerSettings = tlsManagerSettings --{ managerModifyRequest = \req -> print req >> return req }
  manager <- newManager managerSettings
  dbConn <- SL.open openFEC_SqliteFile
  let clientEnv = mkClientEnv manager FEC.baseUrl
      runServant x = runClientM x clientEnv
  loadCandidates runServant dbConn (\x -> FEC.getCandidates [] [] x Nothing Nothing) [2018]

                                      {-
  result <- runClientM query clientEnv
  case result of
    Left err -> putStrLn $ "Query returned an error: " ++ show err
    Right x  -> encodeFile "NY-11.json" x
--    Right x  -> PP.printTable x
  return ()
-}


