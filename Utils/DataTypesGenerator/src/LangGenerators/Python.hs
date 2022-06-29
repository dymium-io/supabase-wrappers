{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module LangGenerators.Python where

import           RIO
import           RIO.FilePath
import           RIO.List          (intercalate, sort, sortOn)
import qualified RIO.Map           as M
import qualified RIO.Text          as T

import qualified Data.Time         as DT
import           NeatInterpolation

import           Types

import qualified Common            as C
import           RIO.Char          (isAlpha, isUpper)
import qualified RIO.Set           as Set

run :: [ModuleDef] -> InstallPath -> RootModule -> Camelizer -> RIO App ()
run mDefs installPath@(InstallPath installPath') _rootModule camelizer = do
  logInfo $ "Installing Python modules to " <> displayShow installPath' <> "..."
  C.createInstallPaths installPath mDefs transformPath
  nameMappers <- genMappers mDefs camelizer
  mapM_ (genModule installPath nameMappers) mDefs

data NameMappers =
  NameMappers
  { modulesPaths     :: Text -> (Text, [FilePath])
  , mNameMapper      :: Text -> Text
  , enumNameMapper   :: Text -> Text
  , enumFldMapper    :: (Text,Text) -> Text
  , structNameMapper :: Text -> Text
  , structFldMapper  :: Text -> Text
  }

genModule :: InstallPath ->
             NameMappers ->
             ModuleDef   ->
             RIO App ()
genModule
  (InstallPath installPath)
  nameMappers
  mDef = do
    logInfo . display $ "  Installing "<>T.pack inst<>"..."
    writeFileUtf8 inst [text|
  # this file is automatically generated.
  # !!! DO NOT EDIT !!!

  from __future__ import annotations
  from typing import Any, List, Optional
  ${importEnum}
  ${importDataclass}

  ${imports}

  ${enumDefs}

  ${structDefs}


  |]
  where
    inst =
      let (fName', fPath) = mPath mDef
          fName = T.unpack fName' <.> "py"
      in
      joinPath (installPath : fPath) </> fName

    importEnum = case () of
      _ | null enums' -> ""
        | otherwise   -> [text|from enum import Enum|]

    importDataclass = case () of
      _ | null structs' -> ""
        | otherwise   -> [text|from dataclasses import dataclass|]

    imports = mconcat $ importString <$> externalModules mDef
      where
        importString em = T.concat ["from ",rPath," import ",fst (mPath mDef)," as ",lName,"\n"]
          where
            lName = mNameMapper nameMappers  em
            rPath = T.pack $ '.' : intercalate "."
                    (transform <$> C.calcRelativePath (snd $ mPath mDef) (snd $ modulesPaths nameMappers em))
            transform = \case
              ".." -> ""
              p    -> p


    enums' = sortOn eName $ enums mDef
    structs' = sortOn sName $ structs mDef

    mName' = mName mDef

    enumDefs = T.intercalate "\n" $ enumDef nameMappers mName' <$> enums'
    structDefs = T.intercalate "\n" $ structDef nameMappers mName' <$> structs'

enumDef :: NameMappers -> T.Text -> EnumDef -> T.Text
enumDef nameMappers _mName' eDef = [text|
  class ${eName'}(str, Enum):
    ${eFlds}
  |]
  where
    eName' = eName eDef
    eFlds = mconcat $ f <$> eValues eDef
    f v = [text|${efn} = ${efv}|]
      where
        efn = enumFldMapper nameMappers (eName',v)
        efv = pyString v

structDef :: NameMappers -> T.Text -> StructDef -> T.Text
structDef nameMappers _mName' sDef = [text|
  @dataclass
  class ${csn}:
    ${fldDefs}
  |]
  where
    sn = sName sDef
    csn = structNameMapper nameMappers sn
    fldDefs = mconcat $ f <$> sFields sDef
      where
        f (fn,Field { moduleName, typ, container }) = [text|${sfn}: ${styp}|]
          where
            sfn = structFldMapper nameMappers fn
            styp = case container of
              Lst k -> T.replicate k "List[" <>naked <> T.replicate k "]"
              Opt   -> "Optional["<>naked<>"]"
              OptLst k -> "Optional["<>T.replicate k "List[" <>naked <> T.replicate k "]"<>"]"
              Naked -> naked
            naked = case typ of
              BoolS              -> "str"
              IntS               -> "str"
              DoubleS            -> "str"
              DollarS            -> "str"
              YearS              -> "str"
              PhoneS             -> "str"
              DateS              -> "str"
              PrimBool           -> "bool"
              PrimInt            -> "int"
              PrimDouble         -> "float"
              PrimString         -> "str"
              Enum en _ _  -> case moduleName of
                Nothing -> enumNameMapper nameMappers en
                Just mn -> mNameMapper nameMappers  mn<>"."<>enumNameMapper nameMappers en
              Struct stn         -> case moduleName of
                Nothing -> structNameMapper nameMappers stn
                Just mn -> mNameMapper nameMappers  mn<>"."<>structNameMapper nameMappers stn

genMappers :: [ModuleDef] -> Camelizer -> RIO App NameMappers
genMappers mDefs camelizer = do
  checkStructFldNames
  pure $ NameMappers
    { modulesPaths
    , mNameMapper = C.hashMapper mDefs
    , enumNameMapper
    , enumFldMapper
    , structNameMapper = camelizer
    , structFldMapper = sanitize
    }
  where

    checkStructFldNames :: RIO App ()
    checkStructFldNames = case () of
      _ | null conflictingFldDefs -> pure ()
        | otherwise -> do
        logError "Python types generator error: field name is a Python keyword:"
        forM_ (sort conflictingFldDefs) $
          \(mn, (sn, fn)) -> logError $ display $ T.concat ["  ",mn,".",sn,".",fn]
        exitFailure
      where
        conflictingFldDefs = [ (mName mDef,sn) | mDef <- mDefs
                                               , sn <- checkStruct (structs mDef) ]
        checkStruct sDefs = [ (sName sDef,fn) | sDef <- sDefs
                                              , fn <- checkField (sFields sDef) ]
        checkField fDefs = [ fn | (fn,_) <- fDefs, fn `Set.member` pyKeywords ]

    modulesPaths :: T.Text -> (T.Text, [FilePath])
    modulesPaths =
      let
        m = M.fromList $ (mName &&& mPath) <$> mDefs
      in
      \mn -> fromMaybe (error $ "can not find path to module `"<>T.unpack mn<>"`")
             (mn `M.lookup` m)

    enumNameMapper :: Text -> Text
    enumNameMapper = camelizer

    enumFldMapper :: (Text,Text) -> Text
    enumFldMapper (eName', fName') = efm (sanitize . camelizer $ fName')
      where
        efm cfn
          | cfn `Set.member` pyKeywords = prefixize cfn
          | Just (c,_) <- T.uncons cfn, isAlpha c = cfn
          | otherwise = prefixize cfn
        prefixize cfn =
          let en = enumNameMapper eName'
              prefixCandidate = T.filter isUpper en
              prefix = case () of
                _ | T.null prefixCandidate -> T.toUpper $ T.take 1 en
                  | otherwise              -> prefixCandidate
          in prefix<>"_"<>cfn

pyKeywords :: Set.Set Text
pyKeywords = Set.fromList [
  "False",   "None",     "True",     "and",    "as",   "assert", "break",
  "class",   "continue", "def",      "del",    "elif", "else",   "except",
  "finally", "for",      "from",     "global", "if",   "import", "in",
  "is",      "lambda",   "nonlocal", "not",    "or",   "pass",   "raise",
  "return",  "try",      "while",    "with",   "yield"
  ]

pyString :: T.Text -> T.Text
pyString = T.cons '"' . flip T.snoc '"'

pyShow :: Show a => a -> T.Text
pyShow = pyString . T.pack . show

pyEmptyString :: T.Text
pyEmptyString = "\"\""

goNull :: T.Text
goNull = "null"

goDate :: DT.Day -> T.Text
goDate = pyString . T.pack . DT.formatTime DT.defaultTimeLocale "%0m/%0d/%0Y"

sanitize :: Text -> Text
sanitize =
  T.map (\case
            '-' -> '_'
            '*' -> '_'
            '!' -> '_'
            c   -> c)

transformPath :: FilePath -> FilePath
transformPath = id
