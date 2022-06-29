module Types.Ddt where

import           RIO

import           Data.Yaml
import qualified RIO.Map   as M

data DdtDef
  = DdtDef
    { ddtLanguage   :: !Text
    , ddtRootPath   :: !FilePath
    , ddtRootModule :: !(Maybe Text)
    , ddtMapFile    :: !(Maybe Text)
    , ddtModules    :: ![DdtModuleDef]
    } deriving(Show, Eq)

data DdtModuleDef
  = DdtModuleDef
    { ddtSource      :: !FilePath
    , ddtDestination :: !(Maybe FilePath)
    , ddtMod         :: !(Map Text DdtMod)
    } deriving(Show, Eq)

data DdtMod
  = DdtMod
    { ddtPath :: Maybe FilePath
    , ddtName :: Maybe Text
    }  deriving(Show, Eq)

instance ToJSON DdtMod where
  toJSON o =
    object
    [ "destination" .= ddtPath o
    , "rename" .= ddtName o
    ]
instance FromJSON DdtMod where
  parseJSON = withObject "DdtMod" $ \v ->
    DdtMod
    <$> v .:? "destination"
    <*> v .:? "rename"

instance ToJSON DdtModuleDef where
  toJSON o =
    object
    [ "source" .= ddtSource o
    , "destination" .= ddtDestination o
    , "submodules" .=
      (let m = ddtMod o in
        if M.null $ ddtMod o
        then Nothing
        else Just m
      )
    ]
instance FromJSON DdtModuleDef where
  parseJSON = withObject "ModuleDef" $ \v ->
    DdtModuleDef
    <$> v .:  "source"
    <*> v .:? "destination"
    <*> v .:? "submodules" .!= M.empty

instance ToJSON DdtDef where
  toJSON o =
    object
    [ "language" .= ddtLanguage o
    , "path" .= ddtRootPath o
    , "root-module" .= ddtRootModule o
    , "map-file" .= ddtMapFile o
    , "modules" .= ddtModules o
    ]

instance FromJSON DdtDef where
  parseJSON = withObject "DdtDef" $ \v ->
    DdtDef
    <$> v .:  "language"
    <*> v .:  "path"
    <*> v .:? "root-module"
    <*> v .:? "map-file"
    <*> v .:  "modules"
