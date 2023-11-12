{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
    ( OptsMigrate
    , OptsDropSchema
    , OptsConfirmChecksums
    , OptsRepairChecksum
    , OptsRegisterMigrations
    , OptsUnregisterMigrations
    , OptsVersion
    , Command (..)

    -- Shared Lenses
    , rootDirectory
    , postgreSettings
    , postgreSchema

    -- Migrate Lenses
    , applyFlag
    , runTestsFlag

    -- Repair Lenses
    , migrationName

    -- Register/Unregister Migrations Lenses
    , registerMigrations
    , unregisterMigrations

    , configParser
    ) where

import           Control.Lens              hiding (argument)
import           Data.Char
import qualified Data.Text                 as T
import qualified Hasql.Connection          as Sql
import           Options.Applicative
import           Data.String

class HasRootDirectory a where rootDirectory :: Lens' a T.Text
class HasPostgreSettings a where postgreSettings :: Lens' a Sql.Settings
class HasPostgreSchema a where postgreSchema :: Lens' a T.Text

data OptsMigrate
    = OptsMigrate
        { _omigratePostgreSchema   :: !T.Text
        , _omigrateRootDirectory   :: !T.Text
        , _omigrateApply           :: !Bool
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
        { _oregisterPostgreSchema   :: !T.Text
        , _oregisterRootDirectory   :: !T.Text
        , _oregisterPostgreSettings :: !Sql.Settings
        , _oregisterMigrations      :: Either T.Text [T.Text]
        }
    deriving (Show)

$(makeClassy ''OptsRegisterMigrations)

instance HasRootDirectory OptsRegisterMigrations where rootDirectory = oregisterRootDirectory
instance HasPostgreSettings OptsRegisterMigrations where postgreSettings = oregisterPostgreSettings
instance HasPostgreSchema OptsRegisterMigrations where postgreSchema = oregisterPostgreSchema

registerMigrations :: Lens' OptsRegisterMigrations (Either T.Text [T.Text])
registerMigrations = oregisterMigrations

--

data OptsUnregisterMigrations
    = OptsUnregisterMigrations
        { _ounregisterPostgreSchema   :: !T.Text
        , _ounregisterPostgreSettings :: !Sql.Settings
        , _ounregisterMigrations      :: Either T.Text [T.Text]
        }
    deriving (Show)

$(makeClassy ''OptsUnregisterMigrations)

instance HasPostgreSettings OptsUnregisterMigrations where postgreSettings = ounregisterPostgreSettings
instance HasPostgreSchema OptsUnregisterMigrations where postgreSchema = ounregisterPostgreSchema

unregisterMigrations :: Lens' OptsUnregisterMigrations (Either T.Text [T.Text])
unregisterMigrations = ounregisterMigrations

--

data OptsDropSchema
    = OptsDropSchema
        { _odropPostgreSchema   :: !T.Text
        , _odropPostgreSettings :: !Sql.Settings
        }
    deriving (Show)

$(makeClassy ''OptsDropSchema)

instance HasPostgreSettings OptsDropSchema where postgreSettings = odropPostgreSettings
instance HasPostgreSchema OptsDropSchema where postgreSchema = odropPostgreSchema

--

data OptsVersion
    = OptsVersion
    deriving (Show)

data Command
    = CmdMigrate OptsMigrate
    | CmdDropSchema OptsDropSchema
    | CmdConfirmChecksums OptsConfirmChecksums
    | CmdRepairChecksum OptsRepairChecksum
    | CmdRegisterMigrations OptsRegisterMigrations
    | CmdUnregisterMigrations OptsUnregisterMigrations
    | CmdVersion OptsVersion
    deriving (Show)

configParser :: ParserInfo Command
configParser = info (commandParser <**> helper)
    ( fullDesc
    <> header "mallard - Database management for pedantic people." )

commandParser :: Parser Command
commandParser = subparser
    (  command "migrate" infoMigrateParser
    <> command "drop" infoDropSchemaParser
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
    <*> flag False True (long "test" <> short 't' <> help "Run tests after migration.")
    <*> connectionSettings id)

--

infoDropSchemaParser :: ParserInfo Command
infoDropSchemaParser = info (cmdDropSchemaParser <**> helper)
    (progDesc "Drop schema.")

cmdDropSchemaParser :: Parser Command
cmdDropSchemaParser = CmdDropSchema <$> (OptsDropSchema
    <$> schemaArgument
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
    <> command "unregister-migrations" infoUnregisterMigrationsParser
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
    <$> schemaArgument
    <*> rootArgument
    <*> connectionSettings id
    <*> (stdInput <|> fileInput <|> argsInput))
  where
    stdInput =
      flag' (Left "-") (long "from-stdin" <>
                        help "Read (sub)list of migrations from standard input")
    fileInput =
      Left <$>
      strOption ( long "from-file" <> short 'f' <> metavar "FILE" <>
                  help "Read (sub)list of migrations from file." )
    argsInput =
      Right <$>
      some (strArgument (metavar "MIGRATION_NAME..." <>
                         help "List of migrations"))

--

infoUnregisterMigrationsParser :: ParserInfo Command
infoUnregisterMigrationsParser = info (cmdUnregisterMigrationsParser <**> helper)
    (progDesc "Unregister migrations supplied by scripts.")

cmdUnregisterMigrationsParser :: Parser Command
cmdUnregisterMigrationsParser = CmdUnregisterMigrations <$> (OptsUnregisterMigrations
    <$> schemaArgument
    <*> connectionSettings id
    <*> (stdInput <|> fileInput <|> argsInput))
  where
    stdInput =
      flag' (Left "-") (long "from-stdin" <>
                        help "Read (sub)list of migrations from standard input")
    fileInput =
      Left <$>
      strOption ( long "from-file" <> short 'f' <> metavar "FILE" <>
                  help "Read (sub)list of migrations from file." )
    argsInput =
      Right <$>
      some (strArgument (metavar "MIGRATION_NAME..." <>
                         help "List of migrations"))

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

-- | Given a function, which updates the long names produces a parser
-- of @Hasql.Connection.'A.Settings'@.
--
-- You can use this function to prefix the name or you can just specify 'id',
-- if you don't want it changed.
connectionSettings :: (String -> String) -> Parser Sql.Settings
connectionSettings updatedName =
  Sql.settings <$> host <*> port <*> user <*> password <*> database
  where
    host =
      fmap fromString $
        strOption $
          long (updatedName "host")
            <> value "127.0.0.1"
            <> showDefault
            <> help "Server host"
    port =
      option auto $
        long (updatedName "port")
          <> value 5432
          <> showDefault
          <> help "Server port"
    user =
      fmap fromString $
        strOption $
          long (updatedName "user")
            <> value "postgres"
            <> showDefault
            <> help "Username"
    password =
      fmap fromString $
        strOption $
          long (updatedName "password")
            <> value ""
            <> showDefault
            <> help "Password"
    database =
      fmap fromString $
        strOption $
          long (updatedName "database")
            <> help "Database name"

