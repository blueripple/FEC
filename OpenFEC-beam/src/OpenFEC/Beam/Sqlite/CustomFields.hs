{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module OpenFEC.Beam.Sqlite.CustomFields where

import qualified Database.Beam                    as B
import qualified Database.Beam.Backend.SQL        as B
import qualified Database.Beam.Migrate            as B
import qualified Database.Beam.Sqlite             as B
import qualified Database.Beam.Sqlite.Syntax      as B
import qualified Database.SQLite.Simple.FromField as SL

import           OpenFEC.Beam.Types               (Office, Party,
                                                   SpendingIntention)

import           Text.Read                        (readMaybe)

-- marshall to DB as string via show.
instance B.HasSqlValueSyntax be String => B.HasSqlValueSyntax be Party where
  sqlValueSyntax = B.autoSqlValueSyntax

instance B.HasSqlValueSyntax be String => B.HasSqlValueSyntax be Office where
  sqlValueSyntax = B.autoSqlValueSyntax

instance B.HasSqlValueSyntax be String => B.HasSqlValueSyntax be SpendingIntention where
  sqlValueSyntax = B.autoSqlValueSyntax

-- marshall back by reading as String and then readMaybe
instance SL.FromField Party where
  fromField f = do
    x <- readMaybe <$> SL.fromField f
    case x of
      Nothing -> SL.returnError SL.ConversionFailed f "Could not 'read' value for Party"
      Just x -> pure x

instance SL.FromField Office where
  fromField f = do
    x <- readMaybe <$> SL.fromField f
    case x of
      Nothing -> SL.returnError SL.ConversionFailed f "Could not 'read' value for Office"
      Just x -> pure x

instance SL.FromField SpendingIntention where
  fromField f = do
    x <- readMaybe <$> SL.fromField f
    case x of
      Nothing -> SL.returnError SL.ConversionFailed f "Could not 'read' value for Office"
      Just x -> pure x

instance B.FromBackendRow B.Sqlite Party
instance B.FromBackendRow B.Sqlite Office
instance B.FromBackendRow B.Sqlite SpendingIntention

instance B.HasDefaultSqlDataType B.SqliteDataTypeSyntax Party where
  defaultSqlDataType _ _ = B.sqliteTextType

instance B.HasDefaultSqlDataType B.SqliteDataTypeSyntax Office where
  defaultSqlDataType _ _ = B.sqliteTextType

instance B.HasDefaultSqlDataType B.SqliteDataTypeSyntax SpendingIntention where
  defaultSqlDataType _ _ = B.sqliteTextType

instance B.HasDefaultSqlDataTypeConstraints B.SqliteColumnSchemaSyntax Party
instance B.HasDefaultSqlDataTypeConstraints B.SqliteColumnSchemaSyntax Office
instance B.HasDefaultSqlDataTypeConstraints B.SqliteColumnSchemaSyntax SpendingIntention

{-
--This doesn't belong here but is currently unused and I want to have a note of the syntax
-- in case I need to do thinsg this way in the future

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
