{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import qualified Config                                       as C
import           Control.Lens                                 hiding (argument,
                                                               noneOf)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Text.Lens                               hiding (text)
import           Database.Mallard
import qualified Database.Mallard.Commands.ConfirmChecksums   as Commands
import qualified Database.Mallard.Commands.Migrate            as Commands
import qualified Database.Mallard.Commands.RegisterMigrations as Commands
import qualified Database.Mallard.Commands.RepairChecksum     as Commands
import qualified Hasql.Pool                                   as Pool
import           Options.Applicative
import           Path
import           Path.IO
import           System.IO                                    (stdin)

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
        C.CmdMigrate o            -> mainMigrate o
        C.CmdConfirmChecksums o   -> mainConfirmChecksums o
        C.CmdRepairChecksum o     -> mainRepairChecksum o
        C.CmdRegisterMigrations o -> mainRegisterMigrations o
        C.CmdVersion o            -> mainVersion o

mainVersion :: C.OptsVersion -> IO ()
mainVersion _ = putStrLn "mallard -- 0.6.3.0"

--

mainMigrate :: C.OptsMigrate -> IO ()
mainMigrate appOpts = ( do
    pool <- Pool.acquire (1, 30, appOpts ^. C.postgreSettings)

    root <- parseRelOrAbsDir (appOpts ^. C.rootDirectory . unpacked)
    Commands.migrate pool
      root
      (appOpts ^. C.postgreSchema)
      (appOpts ^. C.applyFlag)
      (appOpts ^. C.ensureSchemaFlag)
      (appOpts ^. C.runTestsFlag)

    Pool.release pool ) `catch` (\e -> putStrLn $ "=> [" <> displayException (e :: SomeException) <> "]")

--

mainConfirmChecksums :: C.OptsConfirmChecksums -> IO ()
mainConfirmChecksums appOpts = do
    pool <- Pool.acquire (1, 30, appOpts ^. C.postgreSettings)

    root <- parseRelOrAbsDir (appOpts ^. C.rootDirectory . unpacked)
    Commands.confirmChecksums pool root (appOpts ^. C.postgreSchema)

    Pool.release pool

--

mainRepairChecksum :: C.OptsRepairChecksum -> IO ()
mainRepairChecksum appOpts = do
    pool <- Pool.acquire (1, 30, appOpts ^. C.postgreSettings)

    root <- parseRelOrAbsDir (appOpts ^. C.rootDirectory . unpacked)
    Commands.repairChecksum pool root (MigrationId (appOpts ^. C.migrationName))

    Pool.release pool

mainRegisterMigrations :: C.OptsRegisterMigrations -> IO ()
mainRegisterMigrations appOpts = do
    pool <- Pool.acquire (1, 30, appOpts ^. C.postgreSettings)

    root <- parseRelOrAbsDir (appOpts ^. C.rootDirectory . unpacked)

    let migrationsInput =
          case appOpts ^. C.registerMigrationsFileName . unpacked of
            "-" -> Left stdin
            fn  -> Right fn

    Commands.registerMigrations pool
      root
      (appOpts ^. C.postgreSchema)
      migrationsInput

    Pool.release pool
--

parseRelOrAbsDir :: (MonadThrow m, MonadCatch m, MonadIO m) => FilePath -> m (Path Abs Dir)
parseRelOrAbsDir file = parseAbsDir file `catch` (\(_::PathException) -> makeAbsolute =<< parseRelDir file)
