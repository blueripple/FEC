{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
--{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE DataKinds                 #-}
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

import           ElectionResults                          (loadElectionResults)
import           ExploreFEC.Data.Spending                 (CandidateSpending (..),
                                                           describeSpending,
                                                           getCandidateSpending')
import           Forecast538                              (load538ForecastData)

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

import           Data.Aeson                               (encodeFile)
import qualified Data.Foldable                            as F
import qualified Data.FuzzySet                            as FS
import qualified Data.List                                as L
import           Data.List.Split                          (chunksOf, splitOn)
import           Data.List.Unique                         (repeated)
import qualified Data.Map                                 as M
import           Data.Maybe                               (isNothing)
import qualified Data.Sequence                            as Seq
import qualified Data.Text                                as T
import           Data.Time.Format                         (defaultTimeLocale,
                                                           parseTimeM)
import           Data.Time.LocalTime                      (LocalTime)
import qualified Data.Vector                              as V
import           Data.Vinyl                               (ElField (..))
import           Data.Vinyl.Curry                         (runcurryX)
import           Data.Vinyl.Lens                          (rlens, rlens')
import           Network.HTTP.Client                      (Manager, defaultManagerSettings,
                                                           managerModifyRequest,
                                                           newManager)
import           Network.HTTP.Client.TLS                  (tlsManagerSettings)
import           Servant.Client                           (ClientM,
                                                           ServantError,
                                                           mkClientEnv,
                                                           runClientM)

import           Frames

import qualified Text.PrettyPrint.Tabulate                as PP



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
--          liftIO $ putStr $ (T.unpack $ maybe (FEC._committee_id committee) id $ FEC._committee_name committee) ++ " (" ++ (show $ length candidates) ++ ")..."
          _ <- B.runInsert . B.insert (FEC._openFEC_DB_candidate_x_committee FEC.openFEC_DB) $ B.insertValues $ toInsert
          return ()
        totalXRows <- B.runSelectReturningOne $ B.select countCxCRows
        liftIO $ putStrLn $ "Loaded " ++ maybe "0 (Error counting)" show totalXRows ++ " rows."


maxDisbursementId = FEC._disbursement_id <$> (B.limit_ 1 $ B.orderBy_ (B.desc_ . FEC._disbursement_id) $ B.all_ (FEC._openFEC_DB_disbursement FEC.openFEC_DB))
maxIndExpenditureId = FEC._indExpenditure_id <$> (B.limit_ 1 $ B.orderBy_ (B.desc_ . FEC._indExpenditure_id) $ B.all_ (FEC._openFEC_DB_indExpenditure FEC.openFEC_DB))
maxPartyExpenditureId = FEC._partyExpenditure_id <$> (B.limit_ 1 $ B.orderBy_ (B.desc_ . FEC._partyExpenditure_id) $ B.all_ (FEC._openFEC_DB_partyExpenditure FEC.openFEC_DB))

-- reverse makes this order-preserving.  Do we care?
addIds :: Lens' a Int -> Int -> [a] -> [a]
addIds l firstId as = reverse . fst  $ F.foldl' (\(newAs, newId) oldA -> (((l .~ newId) oldA) : newAs, newId + 1)) ([],firstId) as


loadSpendingForCandidate ::
  (forall a. ClientM a -> IO (Either ServantError a)) ->
  SL.Connection ->
  FEC.Candidate ->
  FEC.ElectionYear ->
  IO ()
loadSpendingForCandidate runServant dbConn candidate electionYear = do
  let getAllCxC = B.all_ $ FEC._openFEC_DB_candidate_x_committee FEC.openFEC_DB
      committeeIDsForCandidateID x = B.filter_ (\cxc -> FEC._candidate_x_committee_candidate_id cxc B.==. (B.val_ $ FEC.CandidateKey (FEC._candidate_id x))) getAllCxC
  cxcs :: [FEC.Candidate_x_Committee] <- B.runBeamSqlite dbConn $ B.runSelectReturningList $ B.select $ committeeIDsForCandidateID candidate
  let committeeIDs = (\(FEC.CommitteeKey x) -> x) . FEC._candidate_x_committee_committee_id <$> cxcs
  putStrLn $ "Querying OpenFEC for spending by/for " ++ T.unpack (FEC._candidate_name candidate)
  candidateSpendingE <- runServant $ getCandidateSpending' candidate committeeIDs electionYear []
  case candidateSpendingE of
    Left err -> putStrLn $ "Servant Query returned an error: " ++ show err
    Right candidateSpending -> do
      putStrLn $ T.unpack $ describeSpending candidateSpending
      B.runBeamSqlite dbConn $ do
        let nextId = maybe 0 (+1)
        dNext <- fmap nextId $ B.runSelectReturningOne $ B.select maxDisbursementId
        iNext <- fmap nextId $ B.runSelectReturningOne $ B.select maxIndExpenditureId
        pNext <- fmap nextId $ B.runSelectReturningOne $ B.select maxPartyExpenditureId
        mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_disbursement FEC.openFEC_DB) . B.insertValues) $ chunksOf 80 $ addIds (FEC.disbursement_id) dNext $ V.toList $ _disbursements candidateSpending
        mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_indExpenditure FEC.openFEC_DB) . B.insertValues) $ chunksOf 80 $ addIds (FEC.indExpenditure_id) iNext $ V.toList $ _independentExpenditures candidateSpending
        mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_partyExpenditure FEC.openFEC_DB) . B.insertValues) $ chunksOf 100 $ addIds (FEC.partyExpenditure_id) pNext $ V.toList $ _partyExpenditures candidateSpending

        B.runDelete $ B.delete (FEC._openFEC_DB_candidate_to_load FEC.openFEC_DB) (\cidOnly -> FEC._candidate_id_only cidOnly B.==. (B.val_ $ FEC.CandidateKey . FEC._candidate_id $ candidate))

candidateNameMatchMap :: [(FEC.Name, FEC.State, FEC.District, FEC.CandidateID)] -> M.Map (FEC.State,FEC.District) (FS.FuzzySet, M.Map Text (FEC.Name, FEC.CandidateID))
candidateNameMatchMap cs =
  let lookup x = M.findWithDefault (FS.defaultSet, M.empty) x
      getKey (_,s,d,_) = (s, if d == 0 then 1 else d)
      getName (n,_,_,_) = n
      getId (_,_,_,i) = i
      lastName = fst . T.breakOn ","
      f m t =
        let k = getKey t
            n = getName t
            ln = lastName n
            (fuzzy, idByLastName) = lookup k m
        in M.insert k (FS.add fuzzy ln, M.insert ln (n, getId t) idByLastName) m
  in F.foldl' f M.empty cs

openFEC_SqliteFile = "../DBs/FEC.db"
houseForecastFile = "../data/house_district_forecast.csv"

main :: IO ()
main = do
  let electionYear = 2018
      doDbMigrations = False
      doUpdateCandidates = False
      doUpdateCommittees = False
      doUpdateSpendingWorkTable = False
      doLoadTransactions = False
      doLoad538Data = False
      doLoadElectionResults = True
      doIf doIt action = if doIt then action else return ()
      managerSettings = tlsManagerSettings { managerModifyRequest = \req -> FEC.delayQueries FEC.fecQueryLimit >> {- putStrLn req >> -}  return req }
  manager <- newManager managerSettings
  dbConn <- SL.open openFEC_SqliteFile
  let clientEnv = mkClientEnv manager FEC.baseUrl
      runBeam = B.runBeamSqlite
  let runServant :: forall a. ClientM a -> IO (Either ServantError a)
      runServant x = runClientM x clientEnv
  doIf doDbMigrations $ do
    putStrLn $ "Doing DB migrations, if necessary."
    B.runBeamSqliteDebug putStrLn dbConn $ autoMigrate migrationBackend openFEC_DbMigratable
  doIf doUpdateCandidates $ do
    putStrLn $ "updating candidate table"
    loadCandidates runServant dbConn (\x -> FEC.getCandidates [] [] Nothing Nothing (Just electionYear) x) [electionYear]
  doIf doUpdateCommittees $ do
    putStrLn $ "updating committee table"
    loadCommittees runServant dbConn (FEC.getCommittees Nothing) [electionYear]
  doIf doUpdateSpendingWorkTable $ runBeam dbConn $ do
    liftIO $ putStrLn $ "building tracking table for remaining transaction loading work, if necessary"
    candidates <- B.runSelectReturningList $ B.select $ B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
    let candidateIds = fmap (FEC.CandidateIdOnly . FEC.CandidateKey . FEC._candidate_id) candidates
    mapM_ (B.runInsert . B.insert (FEC._openFEC_DB_candidate_to_load FEC.openFEC_DB) . B.insertValues) $ chunksOf 900 candidateIds
  doIf doLoadTransactions $ do
    candidatesToLoad <- runBeam dbConn $ B.runSelectReturningList $ B.select $ do
      let getAllCandidates = B.all_ (FEC._openFEC_DB_candidate FEC.openFEC_DB)
          getAllCandidatesToLoad = B.all_ (FEC._openFEC_DB_candidate_to_load FEC.openFEC_DB)
      candidate <- getAllCandidates
      idToLoad <- getAllCandidatesToLoad
      B.guard_ (FEC._candidate_id_only idToLoad `B.references_` candidate)
      return candidate
    putStrLn $ "Found " ++ (show $ length candidatesToLoad) ++ " remaining candidates to load data for."
    let numberedCandidates = reverse $ L.zip [1..] $ reverse candidatesToLoad
    forM_ numberedCandidates $ \(num_left,cand) -> do
      putStrLn $ show num_left ++ " remaining."
      loadSpendingForCandidate runServant dbConn cand electionYear
  doIf doLoad538Data $ load538ForecastData dbConn candidateNameMatchMap
  doIf doLoadElectionResults $ loadElectionResults dbConn candidateNameMatchMap
{-
  result <- runClientM query clientEnv
  case result of
    Left err -> putStrLn $ "Query returned an error: " ++ show err
    Right x  -> encodeFile "NY-11.json" x
--    Right x  -> PP.printTable x
  return ()
-}


