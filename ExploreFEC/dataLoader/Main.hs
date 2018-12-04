{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
--{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
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

import           Control.Lens                     ((^.))
import           Control.Monad                    (forM_, mapM_, sequence)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       (encodeFile)
import qualified Data.Foldable                    as F
import qualified Data.List                        as L
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

-- this is not order preserving
-- O (n (log n)) since the sort is and the group and head are O(n)
uniqueListBy :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
uniqueListBy f = fmap L.head . L.groupBy (\x y -> f x == f y) . L.sortBy (\x y -> compare (f x) (f y))

countCandidateRows = B.aggregate_ (\_ -> B.as_ @Int B.countAll_) (B.all_ $ FEC._openFEC_DB_candidate FEC.openFEC_DB)
countCommitteeRows = B.aggregate_ (\_ -> B.as_ @Int B.countAll_) (B.all_ $ FEC._openFEC_DB_committee FEC.openFEC_DB)
countCxCRows = B.aggregate_ (\_ -> B.as_ @Int B.countAll_) (B.all_ $ FEC._openFEC_DB_candidate_x_committee FEC.openFEC_DB)

loadCandidates :: (forall a. ClientM a -> IO (Either ServantError a)) -> SL.Connection -> ([FEC.ElectionYear] -> ClientM (V.Vector FEC.Candidate)) -> [FEC.ElectionYear] -> IO ()
loadCandidates runServant dbConn getCands electionYears = do
  servantRes <- runServant $ getCands electionYears
  case servantRes of
    Left err -> putStrLn $ "Servant Query returned an error: " ++ show err
    Right candidatesV -> do
      let candidatesL = uniqueListBy (FEC._candidate_id) $ V.toList candidatesV
      putStrLn $ "Loading candidates table to DB. Got " ++ show (L.length candidatesL) ++ " unique candidates for election years=" ++ show electionYears
      B.runBeamSqlite dbConn $ do
        mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_candidate FEC.openFEC_DB) . B.insertValues) $ chunksOf 100 candidatesL
        totalRows <- B.runSelectReturningOne $ B.select countCandidateRows
        liftIO $ putStrLn $ "Loaded " ++ maybe "0 (Error counting)" show totalRows ++ " rows."

loadCommittees :: (forall a. ClientM a -> IO (Either ServantError a)) -> SL.Connection -> ([FEC.ElectionYear] -> ClientM (V.Vector (FEC.Committee, [FEC.CandidateID]))) -> [FEC.ElectionYear] -> IO ()
loadCommittees runServant dbConn getCommsWith electionYears = do
  servantRes <- runServant $ getCommsWith electionYears
  case servantRes of
    Left err -> putStrLn $ "Servant Query returned an error: " ++ show err
    Right committeesWithCandidatesV -> do
      let committeeWithCandidatesL = uniqueListBy (FEC._committee_id . fst) $ V.toList committeesWithCandidatesV
      putStrLn $ "Loading committees table to DB. Got " ++ show (L.length committeeWithCandidatesL) ++ " unique committees for election years=" ++ show electionYears
      B.runBeamSqlite dbConn $ do
        mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_committee FEC.openFEC_DB) . B.insertValues) $ chunksOf 100 (fst . unzip $ committeeWithCandidatesL)
        totalCoRows <- B.runSelectReturningOne $ B.select countCommitteeRows
        liftIO $ putStrLn $ "Loaded " ++ maybe "0 (Error counting)" show totalCoRows ++ " rows."
        liftIO $ putStrLn $ "Loading candidate_x_committee table."
--        let candidatesByCommitteeMap = M.fromList . fmap (\(comm,cands) -> (FEC._committee_id comm, cands)) $ committeeWithCandidatesL
        forM_ committeeWithCandidatesL $ \(committee, candidate_ids) -> do
          let getAllCandidates = B.all_ $ FEC._openFEC_DB_candidate FEC.openFEC_DB
              byCandidateIDs x = F.foldl' (\conds cid -> conds B.||. (FEC._candidate_id x B.==. B.val_ cid)) (B.val_ False) candidate_ids
              selectByCandidateIDs = B.filter_ byCandidateIDs getAllCandidates
          candidates <- B.runSelectReturningList $ B.select selectByCandidateIDs
          let toExpr :: FEC.CandidateT f -> FEC.CommitteeT f -> FEC.Candidate_x_CommitteeT f
              toExpr ca co = FEC.Candidate_x_Committee (B.pk ca) (B.pk co)
              toInsert = fmap (flip toExpr committee) candidates
          liftIO $ putStr $ (T.unpack $ maybe (FEC._committee_id committee) id $ FEC._committee_name committee) ++ " (" ++ (show $ length candidates) ++ ")..."
          _ <- B.runInsert . B.insert (FEC._openFEC_DB_candidate_x_committee FEC.openFEC_DB) $ B.insertValues $ toInsert
          return ()
        totalXRows <- B.runSelectReturningOne $ B.select countCxCRows
        liftIO $ putStrLn $ "Loaded " ++ maybe "0 (Error counting)" show totalXRows ++ " rows."

{-
loadSpendingForCandidate ::
  (forall a. ClientM a -> IO (Either ServantError a)) ->
  SL.Connection ->
  (FEC.CandidateID -> FEC.ElectionYear -> ClientM (V.Vector FEC.Disbursement))
  -> FEC.Candidate
  -> FEC.ElectionYear
  -> IO ()
loadSpendingForCandidate runServant dbConn getDisbursements candidate electionYear = do
  let getAllCxC = B.all_ $ FEC._openFEC_DB_candidate_x_committee FEC.openFEC_DB
      committeeIDsForCandidate x = B.filter_ (FEC._candidate_x_committee_candidate_id B.==. (B.val_ $ FEC._candidate_id x)) getAllCxC
  cxcs <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ committeeIDsForCandidateID candidate
  let committeeIDs = FEC._candidate_x_committee_committee_id <$> cxcs
  putStrLn $ "Querying OpenFEC for spending by/for " ++ T.unpack (FEC._candidate_name candidate)
  candidateSpending <- getCandidateSpending' candidate committeeIDs electionYear
  putStrLn $ T.unpack $ describeSpending candidateSpending
-}


openFEC_SqliteFile = "../DBs/FEC-test.db"

main :: IO ()
main = do
  let electionYears = [2018]
      doUpdateCandidates = False
      doUpdateCommittees = False
      doUpdateSpendingWorkTable = True
      doIf doIt action = if doIt then action else return ()
      managerSettings = tlsManagerSettings { managerModifyRequest = \req -> FEC.delayQueries FEC.fecQueryLimit >> {- putStrLn req >> -}  return req }
  manager <- newManager managerSettings
  dbConn <- SL.open openFEC_SqliteFile
  let clientEnv = mkClientEnv manager FEC.baseUrl
      runBeam = B.runBeamSqlite
  let runServant :: forall a. ClientM a -> IO (Either ServantError a)
      runServant x = runClientM x clientEnv
  putStrLn $ "Doing DB migrations, if necessary."
  B.runBeamSqliteDebug putStrLn dbConn $ autoMigrate migrationBackend openFEC_DbMigratable
  putStrLn $ "updating initial candidate and committee tables, if necessary"
  doIf doUpdateCandidates $ loadCandidates runServant dbConn (\x -> FEC.getCandidates [] [] Nothing Nothing Nothing x) [2018]
  doIf doUpdateCommittees $ loadCommittees runServant dbConn (FEC.getCommittees Nothing) [2018]
  putStrLn $ "building tracking table for remaining transaction loading work, if necessary"
  doIf doUpdateSpendingWorkTable $ runBeam dbConn $ do
    candidates <- B.runSelectReturningList $ B.select $ B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
    let candidateIds = fmap (FEC.CandidateIdOnly . FEC.CandidateKey . FEC._candidate_id) candidates
    mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_candidate_to_load FEC.openFEC_DB) . B.insertValues) $ chunksOf 200 candidateIds
  candidatesToLoad <- runBeam dbConn $ B.runSelectReturningList $ B.select $ do
    let getAllCandidates = B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
        getAllCandidatesToLoad = B.all_ (FEC._openFEC_DB_candidate_to_load FEC.openFEC_DB)
    candidate <- getAllCandidates
    idToLoad <- getAllCandidatesToLoad
    B.guard_ (FEC._candidate_id_only idToLoad `B.references_` candidate)
    return candidate

  putStrLn $ "Found " ++ (show $ length candidatesToLoad) ++ " remaining candidates to load data for."



{-
  result <- runClientM query clientEnv
  case result of
    Left err -> putStrLn $ "Query returned an error: " ++ show err
    Right x  -> encodeFile "NY-11.json" x
--    Right x  -> PP.printTable x
  return ()
-}


