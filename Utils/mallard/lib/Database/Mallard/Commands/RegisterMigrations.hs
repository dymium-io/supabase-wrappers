{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}


module Database.Mallard.Commands.RegisterMigrations
    ( registerMigrations
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
import           Path
import           Path.IO

data AppState
    = AppState
        { _statePool   :: H.Pool
        , _stateSchema :: !Text
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePool
instance HasPostgreSchema AppState where postgreSchema = stateSchema


data DependentMigrationMissingException
  = DependentMigrationMissingException
      { _dmmeMigrationName :: MigrationId
      , _dmmeDependsOn     :: MigrationId
      }
      deriving (Show)

instance Exception DependentMigrationMissingException where
    displayException e = [str|
        $tab$Migration: $: _dmmeMigrationName e$ depends on $: _dmmeDependsOn e$, which is missing
    |]

newtype DuplicateMigrationException
  = DuplicateMigrationException
      { _dmeMigrationName :: MigrationId
      }
      deriving (Show)

instance Exception DuplicateMigrationException where
    displayException e = [str|
        $tab$Migration: $:_dmeMigrationName e$ was already applied
    |]


-- | Run all unapplied migrations in a directory.
registerMigrations
    :: (MonadIO m, MonadThrow m, MonadCatch m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> Path b Dir -- ^ Directory which contains migration scripts.
    -> Text -- ^ Postgres Schema where migrates should be done
    -> Either Text [Text] -- ^ Standard input or File with MigrationsId
    -> m ()
registerMigrations conn dir postgresSchema inp = do
    absDir <- makeAbsolute dir
    toApplyIDs <- case inp of
      Left "-" -> liftIO $ getContents <&> parseToRegister
      Left fn  -> liftIO $ readFile (T.unpack fn) <&> parseToRegister
      Right m  -> pure $ m <&> MigrationId

    let initState = AppState conn postgresSchema

    void $ flip runStateT initState $ do
      ensureMigratonSchema
      (mPlanned, _) <- importDirectory absDir
      mApplied <- getAppliedMigrations

      validateAppliedMigrations mPlanned mApplied

      forM_ toApplyIDs $ \mid ->
        case mid `Map.lookup` mApplied of
          Nothing -> return ()
          Just _  -> liftIO $ throw $ DuplicateMigrationException mid

      let knownIDs = toApplyIDs <> Map.keys mApplied

      toApply <- inflateMigrationIds mPlanned toApplyIDs

      forM_ toApply $ \m ->
        forM_ (m ^. migrationRequires) $ \mid -> do
          if mid `elem` knownIDs
          then return ()
          else liftIO $ throw $ DependentMigrationMissingException (m ^. migrationName) mid

      applyMigrations False toApply

    where
      parseToRegister contents = go (lines contents)

      go []                       = []
      go ("" : rest)              = go rest
      go (('-' : '-' : _) : rest) = go rest
      go (l : rest)               = extractMID l : go rest

      extractMID l = MigrationId . T.pack $ takeWhile (/= ' ') l
