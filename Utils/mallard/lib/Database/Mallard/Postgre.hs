{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Database.Mallard.Postgre
    ( HasPostgreConnection (..)
    , HasPostgreSchema (..)
    , DigestMismatchException (..)
    , ensureMigratonSchema
    , ensureSchema
    , dropSchema
    , getAppliedMigrations
    , getAppliedMigration
    , applyMigrations
    , pUnregisterMigrations
    , setChecksum
    , runTests
    , runTest
    ) where

import           Prelude                     hiding (init)

import           Control.Exception
import           Control.Foldl               (hashMap, premap)
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Crypto.Hash
import           Data.Byteable
import           Data.ByteString             (ByteString)
import           Data.Foldable
import           Data.Int
import           Data.Maybe
import           Data.String.Interpolation
import           Data.Text                   (Text, unpack)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Vector
import           Database.Mallard.Types
import           Database.Mallard.Validation
import qualified Hasql.Pool                  as Pool
import           Hasql.Session
import           Hasql.TH
import           Hasql.Transaction           (Transaction)
import qualified Hasql.Transaction           as HT
import qualified Hasql.Transaction.Sessions  as HT
import           System.Exit

class HasPostgreConnection a where
    postgreConnection :: Lens' a Pool.Pool
class HasPostgreSchema a where
    postgreSchema :: Lens' a Text


data MigrationSchemaVersion
    = NotInit
    | MigrationVersion Int64
    deriving (Eq, Show)

instance Ord MigrationSchemaVersion where
    compare NotInit NotInit                           = EQ
    compare NotInit (MigrationVersion _)              = LT
    compare (MigrationVersion _) NotInit              = GT
    compare (MigrationVersion a) (MigrationVersion b) = compare a b

ensureMigratonSchema :: (MonadIO m, MonadState s m, HasPostgreConnection s) => m ()
ensureMigratonSchema = do
    mversion <- getMigrationSchemaVersion
    let toApply =
            case mversion of
                NotInit -> scriptsMigrationSchema
                MigrationVersion v -> drop (fromIntegral (v + 1)) scriptsMigrationSchema
    forM_ toApply $ \a@(version,_) -> do
        runDB $ HT.transaction HT.Serializable HT.Write $ applyMigrationSchemaMigraiton a
        liftIO $ putStrLn $ "Migrator Version: " <> show version

ensureSchema ::  (MonadIO m, MonadThrow m, MonadState s m, HasPostgreConnection s) => Text -> m ()
ensureSchema schema = do
  doesSchemaExist <- runDB $ statement schema [maybeStatement|SELECT true :: bool FROM information_schema.schemata WHERE schema_name = $1 :: text|]
  case doesSchemaExist of
    Nothing -> do
                runDB $ sql ("CREATE SCHEMA "<>T.encodeUtf8 schema)
                liftIO $ putStrLn $ "Schema `"<>unpack schema<>"` created"
    Just _ -> return ()

runDB :: (MonadIO m, MonadState s m, HasPostgreConnection s) => Session a -> m a
runDB session = do
    pool <- gets (^. postgreConnection)
    res <- liftIO $ Pool.use pool session
    case res of
        Left (Pool.ConnectionUsageError (Just e)) -> liftIO $ do
          putStrLn $ unpack $ T.decodeUtf8 e
          exitFailure
        Left (Pool.ConnectionUsageError Nothing) -> liftIO $do
          putStrLn "runDB: connection error"
          exitFailure
        Left (Pool.SessionUsageError (QueryError q params e)) -> liftIO $ do
          putStrLn $ "Query {"<>unpack (T.decodeUtf8 q)<>"}("<>show params<>") failed:"
          case e of
            ClientError (Just es) -> putStrLn $ unpack $ T.decodeUtf8 es
            ClientError Nothing -> pure ()
            ResultError (ServerError code message detail hint _) -> do
              putStrLn $ "Return code: "<> unpack (T.decodeUtf8 code)
              putStrLn $ unpack $ T.decodeUtf8 message
              case detail of
                Just d  -> putStrLn . unpack . T.unlines $ ("  "<>) <$> T.lines (T.decodeUtf8 d)
                Nothing -> pure ()
              case hint of
                Just h  -> putStrLn $ unpack $ T.decodeUtf8 h
                Nothing -> pure ()
            ResultError (UnexpectedResult r) ->
                putStrLn $ "Unexpected Result: "<>unpack r
            ResultError (RowError r c er) ->
                putStrLn $ "Error at row "<>show r<>" column "<>show c<>": "<>show er
            ResultError (UnexpectedAmountOfRows r) ->
                putStrLn $ "Unexpected Amount of Rows: "<>show r
          exitFailure
        Right value                   -> return value

getAppliedMigration
    :: (MonadIO m, MonadState s m, HasPostgreConnection s)
    => MigrationId -> m (Maybe Migration)
getAppliedMigration mid =
    runDB $ statement mid stmt
    where
      stmt =
        dimap
        unMigrationId
        (fmap toMigration)
        [maybeStatement|SELECT name :: text, description :: text, requires :: text[], checksum :: bytea, script_text :: text FROM mallard.applied_migrations
                        WHERE $1 :: text = name
                       |]

getAppliedMigrations
    :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s)
    => m MigrationTable
getAppliedMigrations = do
  schema <- gets (^. postgreSchema)
  runDB $ statement schema $ stmt (premap toMigration' hashMap)
    where
      stmt = [foldStatement|SELECT name :: text, description :: text, requires :: text[], checksum :: bytea, script_text :: text FROM mallard.applied_migrations
                            WHERE $1 :: text = ANY (schema)
                           |]
      toMigration' :: (Text, Text, Data.Vector.Vector Text, ByteString, Text) -> (MigrationId, Migration)
      toMigration' v = let m = toMigration v in (m ^. migrationName, m)

toMigration :: (Text, Text, Data.Vector.Vector Text, ByteString, Text) -> Migration
toMigration (name, description, requires, checksum, script_text) =
  Migration { _migrationName = MigrationId name
            , _migrationDescription = description
            , _migrationRequires = toList $ MigrationId <$> requires
            , _migrationChecksum = fromMaybe
                                   (error "ByteString was incorrect length for selected Digest type." )
                                   (digestFromByteString checksum)
            , _migrationScript = script_text
            }

applyMigrations :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s) => Bool -> [Migration] -> m ()
applyMigrations doApply = mapM_ (applyMigration doApply)

applyMigration :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s) => Bool -> Migration -> m ()
applyMigration doApply m = do
  schema <- gets (^. postgreSchema)
  runDB $ HT.transaction HT.Serializable HT.Write $ do
    HT.sql ("SET search_path TO "<>T.encodeUtf8 schema)
    appliedChecksum <- HT.statement mId
                       [maybeStatement|SELECT checksum :: bytea FROM mallard.applied_migrations WHERE name = $1 :: text|]
    case appliedChecksum of
      Nothing -> do
        when doApply $
          HT.sql (T.encodeUtf8 $ m ^. migrationScript)
        HT.statement (fromMigration schema) insertStmt
      Just ac | ac == toBytes (m ^. migrationChecksum) -> do
        when doApply $
          HT.sql (T.encodeUtf8 $ m ^. migrationScript)
        HT.statement (schema,mId) updateStmt
              | otherwise -> error ("Migration `"<>show mId<>"` was not applied, because of checksum mismatch")
  liftIO $ putStrLn $ "Applied migration: `"<>unpack schema<>"::"<>unpack mId<>"`"
    where
        mId = m ^. migrationName . to unMigrationId
        insertStmt = [resultlessStatement|INSERT INTO mallard.applied_migrations
                                          (name, schema, description, requires, checksum, script_text)
                                          VALUES
                                          ($1 :: text, ARRAY[$2 :: text], $3 :: text, $4 :: text[], $5 :: bytea, $6 :: text)|]
        updateStmt = [resultlessStatement|UPDATE mallard.applied_migrations
                                          SET schema = schema || $1 :: text
                                          WHERE name = $2 :: text
                                          |]
        fromMigration schema =
          ( mId,
            schema,
            m ^. migrationDescription,
            (m ^. migrationRequires <&> unMigrationId) & Data.Vector.fromList,
            m ^. migrationChecksum . to toBytes,
            m ^. migrationScript
          )

pUnregisterMigrations :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s) => [MigrationId] -> m ()
pUnregisterMigrations = mapM_ pUnregisterMigration

pUnregisterMigration :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s) => MigrationId -> m ()
pUnregisterMigration mid = do
  schema <- gets (^. postgreSchema)
  runDB $ HT.transaction HT.Serializable HT.Write $ do
    HT.statement (schema, unMigrationId mid)
      [resultlessStatement|UPDATE mallard.applied_migrations SET schema = array_remove(schema, $1 :: text)
                           WHERE name = $2 :: text |]
    HT.sql "DELETE FROM mallard.applied_migrations WHERE schema = '{}'"


dropSchema :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s) => m ()
dropSchema = do
  schema <- gets (^. postgreSchema)
  doesSchemaExist <- runDB $ statement schema [maybeStatement|SELECT true :: bool FROM information_schema.schemata WHERE schema_name = $1 :: text|]
  case doesSchemaExist of
    Nothing ->
       liftIO $ putStrLn $ "Schema `"<>unpack schema<>"` does not exist."
    Just _ -> doDrop schema
  where
    doDrop schema = do
      runDB $ HT.transaction HT.Serializable HT.Write $ do
        HT.sql ("DROP SCHEMA IF EXISTS "<>T.encodeUtf8 schema<>" CASCADE")
        HT.statement schema
          [resultlessStatement|UPDATE mallard.applied_migrations SET schema = array_remove(schema, $1 :: text)|]
        HT.sql "DELETE FROM mallard.applied_migrations WHERE schema = '{}'"
      liftIO $ putStrLn $ "Schema `"<>unpack schema<>"` dropped"

runTests :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s) => [Test] -> m ()
runTests = mapM_ runTest

runTest :: (MonadIO m, MonadState s m, HasPostgreConnection s, HasPostgreSchema s) => Test -> m ()
runTest t = do
    schema <- gets (^. postgreSchema)
    runDB $ HT.transaction HT.Serializable HT.Write $ do
        HT.sql ("SET search_path TO "<>T.encodeUtf8 schema)
        HT.sql (T.encodeUtf8 (t ^. testScript))
        HT.condemn

applyMigrationSchemaMigraiton :: (Int64, ByteString) -> Transaction ()
applyMigrationSchemaMigraiton (version, script) = do
    HT.sql script
    HT.statement version stmt
    where
        stmt = [resultlessStatement|INSERT INTO mallard.migrator_version (version) VALUES ($1 :: int8)|]

getMigrationSchemaVersion
    :: (MonadIO m, MonadState s m, HasPostgreConnection s)
    => m MigrationSchemaVersion
getMigrationSchemaVersion = runDB $ do
    isInit <- isMigrationVersionZero
    if isInit
        then do
            version <- statement () stmt
            case version of
                Nothing -> return $ MigrationVersion 0
                Just x  -> return $ MigrationVersion x
        else return NotInit
    where
        stmt = [maybeStatement|SELECT coalesce(max(version), 0) :: int8 as max_version FROM mallard.migrator_version|]

isMigrationVersionZero :: Session Bool
isMigrationVersionZero = do
    mtable <- statement () stmt
    case mtable of
        Nothing -> return False
        Just _  -> return True
    where
        stmt = [maybeStatement|SELECT table_name :: text FROM information_schema.tables WHERE table_schema = 'mallard' AND table_name = 'migrator_version'|]

setChecksum :: (MonadIO m, MonadState s m, HasPostgreConnection s) => MigrationId -> Digest a -> m ()
setChecksum mid d = do
  runDB $ statement (toBytes d, unMigrationId mid) stmt
  where
    stmt = [resultlessStatement|UPDATE mallard.applied_migrations SET checksum = $1 :: bytea WHERE name = $2 :: text|]

-- Exceptions

data DigestSizeMismatchException
    = DigestSizeMismatchException
    deriving (Show)

instance Exception DigestSizeMismatchException where
    displayException _ = [str|
        The size of the applied migration's checksum is not valid. This may imply the
        algorithm used by this tool has changed.
    |]

    -- TODO: Add ability to reset all checksums in migration table.
