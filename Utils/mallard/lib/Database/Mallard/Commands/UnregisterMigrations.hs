{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}


module Database.Mallard.Commands.UnregisterMigrations
    ( unregisterMigrations
    ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
import           Data.String.Interpolation
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.Mallard
import qualified Hasql.Pool                 as H

data AppState
    = AppState
        { _statePool   :: H.Pool
        , _stateSchema :: !Text
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePool
instance HasPostgreSchema AppState where postgreSchema = stateSchema


data DependentMigrationException
  = DependentMigrationException
      { _dmeMigrationName :: MigrationId
      , _dmeDependsOn     :: MigrationId
      }
      deriving (Show)

instance Exception DependentMigrationException where
    displayException e = [str|
        $tab$Migration: $: _dmeMigrationName e$ depends on $: _dmeDependsOn e$, which is missing
    |]

newtype MissingMigrationException
  = MissingMigrationException
      { _mmeMigrationName :: MigrationId
      }
      deriving (Show)

instance Exception MissingMigrationException where
    displayException e = [str|
        $tab$Migration: $: _mmeMigrationName e$ is missing
    |]


-- | Run all unapplied migrations in a directory.
unregisterMigrations
    :: (MonadIO m, MonadThrow m, MonadCatch m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> Text -- ^ Postgres Schema where migrates should be done
    -> Either Text [Text] -- ^ Standard input or File with MigrationsId
    -> m ()
unregisterMigrations conn postgresSchema inp = do

    toApplyIDs <- case inp of
      Left "-" -> liftIO $ getContents <&> parseToRegister
      Left fn  -> liftIO $ readFile (T.unpack fn) <&> parseToRegister
      Right m  -> pure $ m <&> MigrationId

    let initState = AppState conn postgresSchema

    void $ flip runStateT initState $ do

      mApplied <- getAppliedMigrations

      forM_ toApplyIDs $ \mid ->
        case mid `Map.lookup` mApplied of
          Nothing -> liftIO $ throw $ MissingMigrationException mid
          Just _  -> return ()

      forM_ mApplied $ \m ->
        unless (m ^. migrationName `elem` toApplyIDs) $ do
          forM_ (m ^. migrationRequires) $ \mid -> do
            when (mid `elem` toApplyIDs) $
              liftIO $ throw $ DependentMigrationException (m ^. migrationName) mid

      pUnregisterMigrations toApplyIDs

    where
      parseToRegister contents = go (lines contents)

      go []                       = []
      go ("" : rest)              = go rest
      go (('-' : '-' : _) : rest) = go rest
      go (l : rest)               = extractMID l : go rest

      extractMID l = MigrationId . T.pack $ takeWhile (/= ' ') l
