{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module LangGenerators.GoLang where

import           RIO
import           RIO.FilePath      (joinPath, splitDirectories, (<.>), (</>))
import           RIO.List          (lastMaybe, nub, sortOn)
import qualified RIO.Map           as M
import qualified RIO.Text          as T

import           Data.Char         (isUpper)
import qualified Data.Time         as DT
import           NeatInterpolation

import           Data.Hashable     (hash)

import qualified Common            as C

import qualified RIO.Set           as Set
import           Types

run :: [ModuleDef] -> InstallPath -> RootModule -> Camelizer -> RIO App ()
run mDefs installPath@(InstallPath installPath') rootModule@(RootModule root) camelizer = do
  case (root, mPath <$> mDefs) of
    (Nothing, h:t) | any (/= h) t  -> do
      logError "GoLang generator: \"root module\" parameter must be defined for packages installed in different directories"
      exitFailure
    _ -> pure ()
  logInfo $ "Installing GoLang packages to " <> displayShow installPath' <> "..."
  C.createInstallPaths installPath mDefs transformPath
  mapM_ (genModule installPath nameMappers) mDefs
    where
      nameMappers = genMappers rootModule mDefs camelizer

data Package
  = Package
  { pPath :: [FilePath]
  , pName :: T.Text
  , pRef  :: T.Text
  } deriving(Eq, Ord, Show)

data NameMappers
  = NameMappers
  { getRoot          :: Maybe Text
  , getPackage       :: Text -> Maybe Package
  , enumNameMapper   :: Text -> (Text -> Text)
  , enumFldMapper    :: Text -> ((Text,Text) -> Text)
  , structNameMapper :: Text -> (Text -> Text)
  , structFldMapper  :: Text -> ((Text, Text) -> Text)
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
    imports <- findImports
    writeFileUtf8 inst [text|
  // this file is automatically generated.
  // !!! DO NOT EDIT !!!

  package ${package'}

  ${imports}

  ${enumDefs}

  ${structDefs}


  |]
  where
    inst =
      let (fName', fPath) = mPath mDef
          fName = T.unpack fName' <.> "go"
      in
      joinPath (installPath : fPath) </> fName

    findImports =
       case lefts imp of
         [] -> case (rights imp, getRoot nameMappers) of
                ([],_)         -> pure ""
                (_,Nothing) -> do
                   logError "GoLang generator: RootModule must be defined"
                   exitFailure
                (imp', Just root) -> pure . T.concat $ aux root <$> (Set.toList . Set.fromList $ imp')
         e  -> do
           logError $ display $
             T.concat ["GoLang generator: module `"
                      ,mName mDef
                      ,"` refers to modules "
                      ,T.pack (show e)
                      ," in the `main` package"
                      ]
           exitFailure
      where
        thisPkg = getPackage nameMappers $ mName mDef
        imp = [ case p of { Nothing -> Left mn; Just pkg -> Right pkg } |
                mn <- externalModules mDef,
                let p = getPackage nameMappers mn,
                p /= thisPkg ]
        aux root p = [text|import ${name} "${path}"|]
          where
            name = pRef p
            path = T.pack $ joinPath $ T.unpack root : pPath p

    package' = maybe "main" pName (getPackage nameMappers $ mName mDef)

    enums' = sortOn eName $ enums mDef
    structs' = sortOn sName $ structs mDef

    mName' = mName mDef

    enumDefs = T.intercalate "\n" $ enumDef nameMappers mName' <$> enums'
    structDefs = T.intercalate "\n" $ structDef nameMappers mName' <$> structs'


enumDef :: NameMappers -> T.Text -> EnumDef -> T.Text
enumDef nameMappers mName' eDef = [text|
  type ${eName'} string
  const (
    ${eFlds}
  )
  |]
  where
    eName' = eName eDef
    eFlds = mconcat $ f <$> eValues eDef
    f v = [text|${efn} ${eName'} = ${efv}|]
      where
        efn = enumFldMapper nameMappers mName' (eName',v)
        efv = goString v

structDef :: NameMappers -> T.Text -> StructDef -> T.Text
structDef nameMappers mName' sDef = [text|
   type ${csn} struct {
     ${fldDefs}
  }
  |]
  where
    sn = sName sDef
    csn = structNameMapper nameMappers mName' sn
    fldDefs = mconcat $ f <$> sFields sDef
      where
        f (fn,Field { moduleName, typ, container }) = [text|${sfn} ${styp} `json:"${fn}"`|]
          where
            sfn = T.map (\case; '-' -> '_'; ch -> ch) $ structFldMapper nameMappers mName' (sn, fn)
            styp = case container of
              Lst k    -> T.replicate k "[]"<>naked
              Opt      -> "*"<>naked
              OptLst k -> "*"<>T.replicate k "[]"<>naked
              Naked    -> naked
            naked = case typ of
              BoolS              -> "string"
              IntS               -> "string"
              DoubleS            -> "string"
              DollarS            -> "string"
              YearS              -> "string"
              PhoneS             -> "string"
              DateS              -> "string"
              PrimBool           -> "bool"
              PrimInt            -> "int"
              PrimDouble         -> "float64"
              PrimString         -> "string"
              Enum en _ _  -> case moduleName of
                Nothing -> enumNameMapper nameMappers mName' en
                Just mn -> case (getPackage nameMappers mName', getPackage nameMappers mn) of
                  (thisPkg, pkg) | thisPkg == pkg ->  enumNameMapper nameMappers mName' en
                  (_, Nothing)       -> error "-- IMPOSSIBLE --"
                  (_, Just pkg)      -> pRef pkg<>"."<>enumNameMapper nameMappers mn en
              Struct stn         -> case moduleName of
                Nothing -> structFldMapper nameMappers mName' (sn,stn)
                Just mn -> case (getPackage nameMappers mName', getPackage nameMappers mn) of
                  (thisPkg, pkg) | thisPkg == pkg ->  structFldMapper nameMappers mName' (sn,stn)
                  (_, Nothing)       -> error "-- IMPOSSIBLE --"
                  (_, Just pkg)      -> pRef pkg<>"."<>structFldMapper nameMappers mn (sn,stn)

genMappers :: RootModule -> [ModuleDef] -> Camelizer -> NameMappers
genMappers (RootModule rootModule) mDefs camelizer =
  NameMappers
    { getRoot
    , getPackage
    , enumNameMapper
    , structNameMapper
    , enumFldMapper
    , structFldMapper = const $ C.capitalizeT . camelizer . sanitizer . snd
    }
  where

    (getRoot, rootPath) = case rootModule of
      Nothing -> (Nothing, [])
      Just r  -> case splitDirectories $ T.unpack r of
        (gr : rp) -> (Just (T.pack gr), rp)
        _         -> error "-- IMPOSSIBLE --"

    getPackage :: T.Text -> Maybe Package
    getPackage =
      let m = M.fromList $ (mName &&& getPkg . mPath) <$> mDefs in
      \mn -> fromMaybe (error $ "can not find path to module `"<>T.unpack mn<>"`")
             (mn `M.lookup` m)
      where
        getPkg :: (Text, [FilePath]) -> Maybe Package
        getPkg (_,path)
          | [] <- path' = Nothing
          | Just p <- lastMaybe path' =
            Just $ Package { pPath = path'
                           , pRef = T.pack $ concat [p,"_",take 8 . drop 1 . show . hash $ path]
                           , pName = T.pack p
                           }
          | otherwise = error "-- IMPOSSIBLE --"
          where path' = rootPath <> path

    enumNameMapper :: T.Text -> (T.Text -> T.Text)
    enumNameMapper =  const camelizer

    enumFldMapper :: T.Text -> ((T.Text, T.Text) -> T.Text)
    enumFldMapper =
      let

        packMapper = foldl' pMap M.empty mDefs
          where
            pMap pm mDef = foldl' (\pm' (en, ef) ->
                                    M.insertWith (<>) (pRef <$> getPackage (mName mDef))
                                    [(mName mDef, (en, ef))]
                                    pm'
                                  )
                           pm
                           (mconcat $ uncurry C.enPairing . (eName &&& eValues) <$> enums mDef)

        m = mconcat (M.toList <$> (C.uniqNames eShortNameGen eLongNameGen <$> M.elems packMapper)) <&>
            (\((mName', (eName', fName')), v) -> (mName', [((eName',fName'),v)])) &
            M.fromListWith (<>) &
            M.map M.fromList &
            M.map (\mE -> fromMaybe (error "-- IMPOSSIBLE --") . (`M.lookup` mE))

      in  fromMaybe (error "-- IMPOSSIBLE --") . (`M.lookup` m)

    structNameMapper :: T.Text -> (T.Text -> T.Text)
    structNameMapper =
      let

        packMapper = foldl' pMap M.empty mDefs
          where
            pMap pm mDef = foldl' (\pm' sn ->
                                    M.insertWith (<>) (pRef <$> getPackage (mName mDef))
                                    [(mName mDef, sn)]
                                    pm'
                                  )
                           pm
                           (sName <$> structs mDef)

        m = mconcat (M.toList <$> (C.uniqNames sShortNameGen sLongNameGen <$> M.elems packMapper)) <&>
            (\((mName', sName'), v) -> (mName', [(sName',v)])) &
            M.fromListWith (<>) &
            M.map M.fromList &
            M.map (\mS -> fromMaybe (error "-- IMPOSSIBLE --") . (`M.lookup` mS))

      in fromMaybe (error "-- IMPOSSIBLE --") . (`M.lookup` m)


    sShortNameGen :: (Text, Text) -> Text
    sShortNameGen = C.capitalizeT  . camelizer . snd

    sLongNameGen :: [(Text,Text)] -> [Text]
    sLongNameGen = ((\s@(mName', _) -> C.capitalizeT mName'<>"_"<>sShortNameGen s) <$>)

    eShortNameGen :: (Text, (Text,Text)) -> Text
    eShortNameGen (mName', (eName', fName')) =
      let en = enumNameMapper mName' eName'
          prefixCandidate = T.filter isUpper en
          prefix = case () of
            _ | T.null prefixCandidate -> T.toUpper $ T.take 1 en
              | otherwise              -> prefixCandidate
      in
      prefix<>"_"<>sanitize fName'

    eLongNameGen :: [(Text, (Text,Text))] -> [Text]
    eLongNameGen c =
      let
        firstCandidate = (\(mName',(eName',fName')) ->
                          enumNameMapper mName' eName'<>"_"<>sanitize fName') <$> c
        secondCandidate = (\e@(mName',_) -> C.capitalizeT mName'<>"_"<>eShortNameGen e) <$> c
        thirdCandidate = (\(mName',(eName',fName')) ->
                          C.capitalizeT mName'<>"_"<>enumNameMapper mName' eName'<>"_"<>sanitize fName') <$> c
      in case () of
        _ | length firstCandidate == length (nub firstCandidate) -> firstCandidate
          | length secondCandidate == length (nub secondCandidate) -> secondCandidate
          | otherwise -> thirdCandidate

    sanitize = sanitizeEnumFld camelizer

goString :: T.Text -> T.Text
goString = T.cons '"' . flip T.snoc '"'

goShow :: Show a => a -> T.Text
goShow = goString . T.pack . show

goEmptyString :: T.Text
goEmptyString = "\"\""

goNull :: T.Text
goNull = "nil"

goDate :: DT.Day -> T.Text
goDate = goString . T.pack . DT.formatTime DT.defaultTimeLocale "%0m/%0d/%0Y"

sanitizeEnumFld :: Camelizer -> Text -> Text
sanitizeEnumFld camelizer ef =
  sanitizer $ (C.capitalizeT . camelizer) ef

sanitizer :: Text -> Text
sanitizer = T.map (\case
                      '-' -> '_'
                      '*' -> '_'
                      '!' -> '_'
                      c   -> c)

transformPath :: FilePath -> FilePath
transformPath = id
