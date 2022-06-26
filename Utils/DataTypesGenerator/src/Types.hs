module Types
       ( App(..)
       , InstallPath(..)
       , RootModule(..)
       , Camelizer
       , module Export
       )
       where

import           RIO
import           RIO.Process

import           Types.Ddt       as Export
import           Types.Lang      as Export
import           Types.ModuleDef as Export

newtype InstallPath = InstallPath { getInstallPath :: FilePath }
newtype RootModule  = RootModule  { getRootModule  :: Maybe Text }
type    Camelizer   = Text -> Text

data App = App
  { verbose           :: !Bool
  , appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL =
    lens appProcessContext (\x y -> x { appProcessContext = y })

