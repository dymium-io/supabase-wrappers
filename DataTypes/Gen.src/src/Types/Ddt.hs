module Types.Ddt where

import           RIO

import           Data.Yaml
import qualified RIO.HashMap as HM

newtype Ddt = Ddt { ddtDefinitions :: [DdtDef] }
              deriving(Show, Eq)

data DdtDef
  = DdtDef
    { ddtSource      :: !FilePath
    , ddtDestination :: !(Maybe FilePath)
    , ddtMod         :: !(HashMap Text DdtMod)
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

instance ToJSON DdtDef where
  toJSON o =
    object
    [ "source" .= ddtSource o
    , "destination" .= ddtDestination o
    , "submodules" .=
      (let m = ddtMod o in
        if HM.null $ ddtMod o
        then Nothing
        else Just m
      )
    ]
instance FromJSON DdtDef where
  parseJSON = withObject "DdtDef" $ \v ->
    DdtDef
    <$> v .:  "source"
    <*> v .:? "destination"
    <*> v .:? "submodules" .!= HM.empty

instance ToJSON Ddt where
  toJSON o =
    object
    [ "definitions" .= ddtDefinitions o
    ]
instance FromJSON Ddt where
  parseJSON = withObject "Ddt" $ \v ->
    Ddt
    <$> v .:  "definitions"
