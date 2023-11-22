{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import qualified Config                                         as C
import           Control.Lens                                   hiding
                                                                 (argument,
                                                                 noneOf)
import           Data.Text.Lens                                 hiding (text)
import           Data.Time.Clock
import           Database.Mallard
import qualified Database.Mallard.Commands.ConfirmChecksums     as Commands
import qualified Database.Mallard.Commands.Drop                 as Commands
import qualified Database.Mallard.Commands.Migrate              as Commands
import qualified Database.Mallard.Commands.RegisterMigrations   as Commands
import qualified Database.Mallard.Commands.RepairChecksum       as Commands
import qualified Database.Mallard.Commands.UnregisterMigrations as Commands
import qualified Hasql.Pool                                     as Pool
import           Options.Applicative
import           Path.IO

-- State

newtype AppState
    = AppState
        { _statePostgreConnection     :: Pool.Pool
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePostgreConnection

-- Application

main :: IO ()
main = do
    modes <- execParser C.configParser
    case modes of
      C.CmdMigrate o              -> mainMigrate o
      C.CmdDropSchema o           -> mainDropSchema o
      C.CmdConfirmChecksums o     -> mainConfirmChecksums o
      C.CmdRepairChecksum o       -> mainRepairChecksum o
      C.CmdRegisterMigrations o   -> mainRegisterMigrations o
      C.CmdUnregisterMigrations o -> mainUnregisterMigrations o
      C.CmdVersion o              -> mainVersion o

mainVersion :: C.OptsVersion -> IO ()
mainVersion _ = putStrLn "mallard -- 0.6.3.0"

--

mainMigrate :: C.OptsMigrate -> IO ()
mainMigrate appOpts = do
    pool <- Pool.acquire 1 (1 :: DiffTime) (10 :: DiffTime) (appOpts ^. C.postgreSettings)

    root <- resolveDir' (appOpts ^. C.rootDirectory . unpacked)
    Commands.migrate pool
      root
      (appOpts ^. C.postgreSchema)
      (appOpts ^. C.applyFlag)
      (appOpts ^. C.runTestsFlag)

    Pool.release pool

--

mainDropSchema :: C.OptsDropSchema -> IO ()
mainDropSchema appOpts = do
    pool <- Pool.acquire 1 (1 :: DiffTime) (10 :: DiffTime) (appOpts ^. C.postgreSettings)

    Commands.dropSchema pool (appOpts ^. C.postgreSchema)

    Pool.release pool

--

mainConfirmChecksums :: C.OptsConfirmChecksums -> IO ()
mainConfirmChecksums appOpts = do
    pool <- Pool.acquire 1 (1 :: DiffTime) (10 :: DiffTime) (appOpts ^. C.postgreSettings)

    root <- resolveDir' (appOpts ^. C.rootDirectory . unpacked)
    Commands.confirmChecksums pool root (appOpts ^. C.postgreSchema)

    Pool.release pool

--

mainRepairChecksum :: C.OptsRepairChecksum -> IO ()
mainRepairChecksum appOpts = do
    pool <- Pool.acquire 1 (1 :: DiffTime) (10 :: DiffTime) (appOpts ^. C.postgreSettings)

    root <- resolveDir' (appOpts ^. C.rootDirectory . unpacked)
    Commands.repairChecksum pool root (MigrationId (appOpts ^. C.migrationName))

    Pool.release pool

mainRegisterMigrations :: C.OptsRegisterMigrations -> IO ()
mainRegisterMigrations appOpts = do
    pool <- Pool.acquire 1 (1 :: DiffTime) (10 :: DiffTime) (appOpts ^. C.postgreSettings)

    root <- resolveDir' (appOpts ^. C.rootDirectory . unpacked)

    Commands.registerMigrations pool
      root
      (appOpts ^. C.postgreSchema)
      (appOpts ^. C.registerMigrations)

    Pool.release pool

mainUnregisterMigrations :: C.OptsUnregisterMigrations -> IO ()
mainUnregisterMigrations appOpts = do
    pool <- Pool.acquire 1 (1 :: DiffTime) (10 :: DiffTime) (appOpts ^. C.postgreSettings)

    Commands.unregisterMigrations pool
      (appOpts ^. C.postgreSchema)
      (appOpts ^. C.unregisterMigrations)

    Pool.release pool
