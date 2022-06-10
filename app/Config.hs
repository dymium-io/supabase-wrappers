{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
    ( OptsMigrate
    , OptsConfirmChecksums
    , OptsRepairChecksum
    , OptsRegisterMigrations
    , OptsVersion
    , Command (..)

    -- Shared Lenses
    , rootDirectory
    , postgreSettings
    , postgreSchema

    -- Migrate Lenses
    , ensureSchemaFlag
    , applyFlag
    , runTestsFlag

    -- Repair Lenses
    , migrationName

    -- Register Migrations Lenses
    , registerMigrationsFileName

    , configParser
    ) where

import           Control.Lens              hiding (argument)
import           Data.Char
import qualified Data.Text                 as T
import qualified Hasql.Connection          as Sql
import           Hasql.OptparseApplicative
import           Options.Applicative

class HasRootDirectory a where rootDirectory :: Lens' a T.Text
class HasPostgreSettings a where postgreSettings :: Lens' a Sql.Settings
class HasPostgreSchema a where postgreSchema :: Lens' a T.Text

data OptsMigrate
    = OptsMigrate
        { _omigratePostgreSchema   :: !T.Text
        , _omigrateRootDirectory   :: !T.Text
        , _omigrateApply           :: !Bool
        , _omigrateCreateSchema    :: !Bool
        , _omigrateRunTests        :: !Bool
        , _omigratePostgreSettings :: !Sql.Settings
        }
    deriving (Show)

$(makeClassy ''OptsMigrate)

instance HasRootDirectory OptsMigrate where rootDirectory = omigrateRootDirectory
instance HasPostgreSettings OptsMigrate where postgreSettings = omigratePostgreSettings
instance HasPostgreSchema OptsMigrate where postgreSchema = omigratePostgreSchema

runTestsFlag :: Lens' OptsMigrate Bool
runTestsFlag = omigrateRunTests

ensureSchemaFlag :: Lens' OptsMigrate Bool
ensureSchemaFlag = omigrateCreateSchema

applyFlag :: Lens' OptsMigrate Bool
applyFlag = omigrateApply

--

data OptsConfirmChecksums
    = OptsConfirmChecksums
        { _oconfirmPostgreSchema   :: !T.Text
        , _oconfirmRootDirectory   :: !T.Text
        , _oconfirmPostgreSettings :: !Sql.Settings
        }
    deriving (Show)

$(makeClassy ''OptsConfirmChecksums)

instance HasRootDirectory OptsConfirmChecksums where rootDirectory = oconfirmRootDirectory
instance HasPostgreSettings OptsConfirmChecksums where postgreSettings = oconfirmPostgreSettings
instance HasPostgreSchema OptsConfirmChecksums where postgreSchema = oconfirmPostgreSchema

--

data OptsRepairChecksum
    = OptsRepairChecksum
        { _orepairRootDirectory   :: !T.Text
        , _orepairMigrationName   :: !T.Text
        , _orepairPostgreSettings :: !Sql.Settings
        }
    deriving (Show)

$(makeClassy ''OptsRepairChecksum)

instance HasRootDirectory OptsRepairChecksum where rootDirectory = orepairRootDirectory
instance HasPostgreSettings OptsRepairChecksum where postgreSettings = orepairPostgreSettings

migrationName :: Lens' OptsRepairChecksum T.Text
migrationName = orepairMigrationName

--

data OptsRegisterMigrations
    = OptsRegisterMigrations
        { _oregisterFileName        :: !T.Text
        , _oregisterPostgreSchema   :: !T.Text
        , _oregisterRootDirectory   :: !T.Text
        , _oregisterPostgreSettings :: !Sql.Settings
        }
    deriving (Show)

$(makeClassy ''OptsRegisterMigrations)

instance HasRootDirectory OptsRegisterMigrations where rootDirectory = oregisterRootDirectory
instance HasPostgreSettings OptsRegisterMigrations where postgreSettings = oregisterPostgreSettings
instance HasPostgreSchema OptsRegisterMigrations where postgreSchema = oregisterPostgreSchema

registerMigrationsFileName :: Lens' OptsRegisterMigrations T.Text
registerMigrationsFileName = oregisterFileName

--

data OptsVersion
    = OptsVersion
    deriving (Show)

data Command
    = CmdMigrate OptsMigrate
    | CmdConfirmChecksums OptsConfirmChecksums
    | CmdRepairChecksum OptsRepairChecksum
    | CmdRegisterMigrations OptsRegisterMigrations
    | CmdVersion OptsVersion
    deriving (Show)

configParser :: ParserInfo Command
configParser = info (commandParser <**> helper)
    ( fullDesc
    <> header "mallard - Database management for pedantic people." )

commandParser :: Parser Command
commandParser = subparser
    ( command "migrate" infoMigrateParser
    <> command "confirm-checksums" infoConfirmChecksumsParser
    <> command "hack" infoHackParser
    <> command "version" infoVersionParser
    )

--

infoMigrateParser :: ParserInfo Command
infoMigrateParser = info (cmdMigrateParser <**> helper)
    (progDesc "Perform database migrations supplied by scripts in a directory structure.")

cmdMigrateParser :: Parser Command
cmdMigrateParser = CmdMigrate <$> (OptsMigrate
    <$> schemaArgument
    <*> rootArgument
    <*> flag False True (long "apply" <> help "Apply all pending migrations.")
    <*> flag False True (long "create-schema" <> help "Create SCHEMA if it does not exist.")
    <*> flag False True (long "test" <> short 't' <> help "Run tests after migration.")
    <*> connectionSettings id)

--

infoConfirmChecksumsParser :: ParserInfo Command
infoConfirmChecksumsParser = info (cmdConfirmChecksumsParser <**> helper)
    (progDesc "Check database migrations for mismatched migration scripts.")

cmdConfirmChecksumsParser :: Parser Command
cmdConfirmChecksumsParser = CmdConfirmChecksums <$> (OptsConfirmChecksums
    <$> schemaArgument
    <*> rootArgument
    <*> connectionSettings id)

--

infoHackParser :: ParserInfo Command
infoHackParser = info (cmdHackParser <**> helper)
    (progDesc "Hacks for managing migrations data manually.")

cmdHackParser :: Parser Command
cmdHackParser = subparser
    (  command "repair-checksum" infoRepairChecksumParser
    <> command "register-migrations" infoRegisterMigrationsParser
    )
--

infoRepairChecksumParser :: ParserInfo Command
infoRepairChecksumParser = info (cmdRepairChecksumParser <**> helper)
    (progDesc "Take a new checksum of migration and replace database entry.")

cmdRepairChecksumParser :: Parser Command
cmdRepairChecksumParser = CmdRepairChecksum <$> (OptsRepairChecksum
    <$> rootArgument
    <*> strArgument (metavar "MIGRATION_NAME")
    <*> connectionSettings id)

--

infoRegisterMigrationsParser :: ParserInfo Command
infoRegisterMigrationsParser = info (cmdRegisterMigrationsParser <**> helper)
    (progDesc "Register migrations supplied by scripts internally, but don't apply them.")

cmdRegisterMigrationsParser :: Parser Command
cmdRegisterMigrationsParser = CmdRegisterMigrations <$> (OptsRegisterMigrations
    <$> strOption ( short 'f' <> metavar "FILE" <>
                    help "File name (or '-' for standard input) with a (sub)list of pending migrations to be registered." )
    <*> schemaArgument
    <*> rootArgument
    <*> connectionSettings id)

--

rootArgument :: Parser T.Text
rootArgument = strOption (long "migrations-root-dir" <> short 'r' <> metavar "ROOT" <>
                          help "Directory containing migration scripts.")

schemaArgument :: Parser T.Text
schemaArgument = option (eitherReader checker)
                 ( long "schema" <> short 's' <>metavar "SCHEMA" <>
                   help "Postgres schema where migrations will be applied.")
  where
    checker :: String -> Either String T.Text
    checker s =
      case T.pack s of
        t | T.all (\c -> isAlphaNum c || c == '-' || c == '_') t -> Right (T.toLower t)
          | otherwise -> Left ("Incorrect schema name: "<>s)
--

infoVersionParser :: ParserInfo Command
infoVersionParser = info (cmdVersionParser <**> helper)
    (progDesc "Display application version.")

cmdVersionParser :: Parser Command
cmdVersionParser = pure (CmdVersion OptsVersion)
