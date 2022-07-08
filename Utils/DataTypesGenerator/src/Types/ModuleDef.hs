module Types.ModuleDef where

import qualified Data.Time as DT
import           RIO

data ModuleDef
  = ModuleDef
    { mPath           :: !(Text,[FilePath])
    , mName           :: !Text
    , externalModules :: ![Text]
    , enums           :: ![EnumDef]
    , structs         :: ![StructDef]
    } deriving(Show)

data EnumDef
  = EnumDef
    { eName   :: !Text
    , eValues :: ![(Text,Maybe Text)]
    , eDflt   :: !(Maybe Text)
    } deriving(Eq, Show)
eEmptyable :: EnumDef -> Bool
eEmptyable = isNothing . eDflt

data StructDef
  = StructDef
    { sName   :: !Text
    , sFields :: ![(Text, Field)]
    } deriving(Show)

data FType
  = BoolS
  | IntS
  | DoubleS
  | DollarS
  | YearS
  | PhoneS
  | DateS
  | PrimBool
  | PrimInt
  | PrimDouble
  | PrimString
  | Enum !Text !(Maybe Text) ![Text]
  | Struct !Text
     deriving(Eq, Show, Ord)

data Container
  = Lst Int
  | OptLst Int
  | Opt
  | Naked
     deriving(Eq,Show)

data Dflt
  = DfltB !Bool
  | DfltI !Int
  | DfltD !Double
  | DfltS !Text
  | DfltDate !DT.Day
  | NoDflt
     deriving(Eq, Show)

data Field
  = Field { moduleName :: Maybe Text
          , typ        :: FType
          , container  :: Container
          , dflt       :: Dflt
          }
     deriving(Eq, Show)
