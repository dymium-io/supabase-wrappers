{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Commands.Migrate
    ( migrate
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
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

-- | Run all unapplied migrations in a directory.
migrate
    :: (MonadIO m, MonadThrow m, MonadCatch m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> Path b Dir -- ^ Directory which contains migration scripts.
    -> Text -- ^ Postgres Schema where migrates should be done
    -> Bool -- ^ Apply flag (if False, just print out a plan)
    -> Bool -- ^ If True, run test scripts after migration.
    -> m ()
migrate conn dir postgresSchema applyFlag runTestsFlag = do
    absDir <- makeAbsolute dir
    let initState = AppState conn postgresSchema

    void $ flip runStateT initState $ do
      ensureMigratonSchema
      when applyFlag (ensureSchema postgresSchema)
      (mPlanned, mTests) <- importDirectory absDir
      mApplied <- getAppliedMigrations
      let mGraph = fromJust $ mkMigrationGraph mPlanned

      validateAppliedMigrations mPlanned mApplied

      let unapplied = getUnappliedMigrations mGraph (Map.keys mApplied)
      toApply <- inflateMigrationIds mPlanned unapplied

      if applyFlag
      then do
        applyMigrations True toApply
        when runTestsFlag $
          runTests (Map.elems mTests)
      else do
        forM_ toApply (printMigration (maximum ((^. migrationName . to (T.length . unMigrationId)) <$> toApply) + 1))
        unless (null toApply) $
          liftIO $ putStrLn "\x1B[31m* These migrations were not applied yet\n* To apply them, add \x1B[1m--apply\x1B[0;31m flag to the command\x1B[0m"
    where
      printMigration l m = liftIO $ putStrLn $
        T.unpack mid <>
        replicate (l - T.length mid) ' ' <>
        "-- " <> (m ^. migrationDescription . to T.unpack)
        where
          mid = m ^. migrationName . to unMigrationId
