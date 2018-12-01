{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Main where


import qualified Database.Beam                as B
import qualified Database.Beam.Migrate        as B
import qualified Database.Beam.Migrate.Simple as B
import qualified Database.Beam.Sqlite         as B
import qualified Database.Beam.Sqlite.Migrate as B
import qualified Database.Beam.Sqlite.Syntax  as B
import qualified Database.SQLite.Simple       as SL

import           OpenFEC.Beam.Types

import           Data.Proxy                   (Proxy (..))

{-
partyFieldType :: B.DataType B.SqliteDataTypeSyntax Party
partyFieldType = B.DataType B.sqliteTextType


migrateCreateTable :: B.Migration B.SqliteCommandSyntax (B.CheckedDatabaseSettings B.Sqlite OpenFEC_DB)
migrateCreateTable = OpenFEC_DB
  <$> B.createTable "candidates"
  (
    CandidateT
    (B.field "_candidate_id" (B.varchar (Just 10)) B.notNull)
    (B.field "_candidate_name" (B.varchar (Just 50)) B.notNull)
    (B.field "_candidate_state" (B.varchar (Just 2)) B.notNull)
    (B.field "_candidate_district" B.int B.notNull)
    (B.field "_candidate_party" partyFieldType B.notNull)
  )
-}
-- HasDefaultSqlDataType(Constraints) instances for any custom field types
instance B.HasDefaultSqlDataType B.SqliteDataTypeSyntax Party where
  defaultSqlDataType _ _ = B.sqliteTextType

instance B.HasDefaultSqlDataTypeConstraints B.SqliteColumnSchemaSyntax Party

-- create the migration
openFEC_DbMigratable :: B.CheckedDatabaseSettings B.Sqlite OpenFEC_DB
openFEC_DbMigratable = B.defaultMigratableDbSettings @B.SqliteCommandSyntax

main :: IO ()
main = do
  conn <- SL.open "../DBs/FEC-test.db"
  B.runBeamSqliteDebug putStrLn conn $ do
    B.autoMigrate B.migrationBackend openFEC_DbMigratable -- do the migration, which creates the tables if new.
    return ()


