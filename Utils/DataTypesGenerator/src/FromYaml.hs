module FromYaml(fromYaml) where

import           Import
import qualified RIO.HashMap                  as HM
import qualified RIO.Map                      as M
import qualified RIO.Set                      as Set

import           RIO.Lens

import           Lens.Micro.Platform          ()

import qualified Data.Graph                   as G

import           Data.Scientific
import qualified Data.Time                    as DT
import           Data.Yaml
import           Lens.Micro.Aeson
import           RIO.List.Partial             (head, tail)

import           RIO.Partial                  (read)
import qualified RIO.Text                     as T
import qualified RIO.Text.Partial             as T
import qualified Text.ParserCombinators.ReadP as P

import qualified Common                       as C
import qualified RIO.Char                     as P
import           RIO.FilePath
import           Text.Read                    (reads)

data SearchResult where
  Err         :: T.Text -> SearchResult
  StructFound :: SearchResult
  EnumFound   :: EnumDef -> SearchResult

fromYaml :: [(DdtModuleDef, [Value])] -> RIO App [ModuleDef]
fromYaml yy' = do
  checkModuleNames yy'
  checkForObjects yy
  let mnames = getModuleNamePath <$> yy
  case mnames ^..each ._Left :: [T.Text] of
    [] ->
      let v = zip (mnames ^..each ._Right) (mconcat $ snd <$> yy')
          mDefs = moduleDef (searchGen v) <$> v
      in
      case mDefs ^..each ._Left of
      [] -> do
        let mms = mDefs ^..each ._Right
        checkCircularDependencies mms
        pure mms
      e  -> do
        mapM_ (logError . display) $ mconcat e
        exitFailure
    e -> do
        mapM_ (logError . display) e
        exitFailure
  where
    yy :: [(DdtModuleDef, Value)]
    yy = mconcat $ uncurry C.enPairing <$> yy'

checkModuleNames :: [(DdtModuleDef, [Value])] -> RIO App ()
checkModuleNames modvals =
  case [ ddtSource md |
         (md, v) <- modvals, checkMName v ] of
    [] -> pure ()
    o  -> do
      logError "\x1B[31mError\x1B[0m:"
      forM_ o $ \ src ->
        logError $ "  in file "<>displayShow src<>": module name is not defined"
      exitFailure
  where
    checkMName :: [Value] -> Bool
    checkMName v = not $ null [ () | m <- v, let mn = m ^? key "module" . _String, isNothing mn ]

checkForObjects :: [(DdtModuleDef, Value)] -> RIO App ()
checkForObjects modvals =
  case [ (ddtSource md, v ^. key "module" . _String, objs) |
         (md, v) <- modvals, let objs = getObjs v, not (null objs) ] of
    [] -> pure ()
    o  -> do
      logError "\x1B[31mError\x1B[0m:"
      forM_ o $ \ (src, m, objs) ->
        logError $ "  in file "<>displayShow src<>": module "<>displayShow m<>": struct(s) "<>displayShow objs
      logError "defined as Objects. Did you forget '-'?"
      exitFailure
  where
    getObjs = (^.. _Object .to HM.toList .each .filtered (has $ _2 ._Object) . _1)

checkCircularDependencies :: [ModuleDef] -> RIO App ()
checkCircularDependencies mms =
  let scc' = G.stronglyConnComp [ ( mName m, mName m, externalModules m ) | m <- mms ]
      scc  = [ unCycle c | c <- scc', isCyclic c ]
      isCyclic (G.CyclicSCC _)  = True
      isCyclic (G.AcyclicSCC _) = False
      unCycle (G.CyclicSCC v) = v
      unCycle _               = error "* IMPOSSIBLE *"

  in case scc of
    [] -> pure ()
    _  -> do
      logError "\x1B[31mError\x1B[0m:"
      forM_ scc $ \ v ->
        logError $ "Cyclic dependency in modules: " <> displayShow (T.intercalate " -> " (v <> [ head v ]))
      exitFailure


searchGen :: [((T.Text,(T.Text, Maybe FilePath)),Value)]
          -> T.Text -> T.Text -> SearchResult
searchGen yy = \mn s -> case mn `M.lookup` m of
  Nothing      -> Err $ "Module `"<>mn<>"` not defined"
  Just (mE,mS) -> case (s `M.lookup` mE, s `Set.member` mS) of
    (Nothing, False) -> Err $ "Neither enum nor struct `"<>s<>"` is defined in module `"<>mn<>"`"
    (Just _,  True)  -> Err $ "Both enum and struct `"<>s<>"` are defined in module `"<>mn<>"`"
    (Just e,  False) -> EnumFound e
    (Nothing, True)  -> StructFound
  where
    m = M.fromList $ (fst . fst &&& ((M.fromList . getEnums  &&&
                                      Set.fromList . getListOfStructs) . snd)) <$> yy

    getEnums :: Value -> [(T.Text, EnumDef)]
    getEnums =
      (^.. _Object .to HM.toList .each .filtered (has $ _2 .nth 0 ._String) .to parseEnum)

    getListOfStructs :: Value -> [T.Text]
    getListOfStructs =
      (^.. _Object .to HM.toList .each .filtered (has $ _2 .nth 0 ._Object) ._1)

moduleDef :: (T.Text -> T.Text -> SearchResult) ->
             ((T.Text, (T.Text, Maybe FilePath)), Value) ->
             Either [T.Text] ModuleDef
moduleDef search ((mName',(renamedModuleName, mPath')),y) =
  let structs' = y ^.. _Object .to HM.toList .each .filtered (has $ _2 .nth 0 ._Object) <&> parseStruct' in
  case structs' ^..each ._Left of
    [] ->
       let (structs, externalModules) = (id &&& externalModules') $ structs' ^..each ._Right in
       Right $ ModuleDef { mPath = (renamedModuleName,
                                    maybe [] splitDirectories mPath')
                         , mName = mName'
                         , externalModules
                         , enums
                         , structs
                         }
    e  -> Left $ (T.snoc mName' '.'<>) <$> mconcat e
  where

    enums =
      y ^.. _Object .to HM.toList .each .filtered (has $ _2 .nth 0 ._String)
      ._1 .to (\case {EnumFound e -> e; _ -> error "-- IMPOSSIBLE --" } . search mName')

    parseStruct' :: (T.Text, Value) -> Either [T.Text] StructDef
    parseStruct' (n, v) =
      let flds = v ^.. _Array .traverse ._Object .to HM.toList ._head .to (cmbn . second (parseField search mName')) in
      case flds ^..each ._Left of
        []  -> Right $ StructDef { sName = n
                                 , sFields = flds ^..each ._Right
                                 }
        e   -> Left $ (\(fn, e') -> n<>"."<>fn<>": "<>e') <$> e
      where
        cmbn :: (n, Either a b) -> Either (n,a) (n,b)
        cmbn (c, Left a)  = Left (c,a)
        cmbn (c, Right b) = Right (c,b)

    externalModules' s =
      Set.toList . mconcat $ modRefs . sFields <$> s
        where
          modRefs :: [(T.Text, Field)] -> Set.Set T.Text
          modRefs = foldMap (maybe Set.empty Set.singleton . moduleName . snd)

getModuleNamePath :: (DdtModuleDef,Value)
                  -> Either T.Text (T.Text, (T.Text, Maybe FilePath))
getModuleNamePath (ddt,y) =
  case y ^? key "module" . _String of
    Nothing ->  Left $ "module name is not defined in "<>T.pack (ddtSource ddt)
    Just mn -> Right $ case mn `M.lookup` ddtMod ddt of
      Nothing -> (mn, (mn, mp))
      Just md -> (mn, (fromMaybe mn . ddtName &&& ddtPath) md)
  where
    mp = ddtDestination ddt

parseEnum :: (T.Text, Value) -> (T.Text, EnumDef)
parseEnum (eName', y') =
  case y' ^. nth 0 . _String :: T.Text of
    "<EMPTY>" ->
      (eName', EnumDef { eName = eName'
                       , eDflt = Nothing
                       , eValues = tail $ y' ^.. _Array .traverse ._String .to parseValue
                       })
    dflt' ->
      (eName', EnumDef { eName = eName'
                       , eDflt = Just dflt'
                       , eValues = y' ^.. _Array .traverse ._String .to parseValue
                       })
  where
    parseValue :: Text -> (Text, Maybe Text)
    parseValue s =
      let (v', descr) = T.breakOn "<*>" s
          v = T.strip v'
      in
      if T.null descr
      then (v,Nothing)
      else (v,Just . T.strip . T.drop 3 $ descr)

parseField :: (T.Text -> T.Text -> SearchResult)
              -> T.Text
              -> Value
              -> Either T.Text Field
parseField search mName' v =
  case v of
    String str -> parseStr search mName' str
    Object obj -> parseObj search mName' obj
    _          -> Left . T.pack $ "Can not parse field `"<>show v<>"`"

parseStr :: (T.Text -> T.Text -> SearchResult)
            -> T.Text
            -> T.Text
            -> Either T.Text Field
parseStr search mName' str
  | Just ('?', s) <- T.uncons str =
    case () of
      _ | [((m, t, i),"")] <- P.readP_to_S ((,,) <$> modSpec <*> typeSpec <*> initSpec <* P.eof) (T.unpack s) ->
        let moduleName = T.pack <$> m in
        case resolveType' moduleName (T.pack t) Opt (T.pack $ fromMaybe "" i) of
          Right (typ, container, dflt) -> Right $ Field { moduleName , typ , container , dflt }
          Left e -> Left e
        | [((k, m, t),"")] <- P.readP_to_S listSpec (T.unpack s) ->
          let moduleName = T.pack <$> m in
          case resolveType' moduleName (T.pack t) (OptLst k) "" of
            Right (typ, container, dflt) -> Right $ Field { moduleName, typ, container, dflt }
            Left e -> Left e
        | otherwise -> Left $ "Invalid spcification of optional field "<>str
  | Just ('<', _) <- T.uncons str =
    case P.readP_to_S listSpec (T.unpack str) of
      [((k, m, t),"")] ->
        let moduleName = T.pack <$> m in
        case resolveType' moduleName (T.pack t) (Lst k) "" of
          Right (typ, container, dflt) -> Right $ Field { moduleName, typ, container, dflt }
          Left e -> Left e
      _ -> Left $ "Invalid spcification of list field "<>str
  | otherwise =
    case P.readP_to_S ((,,) <$> modSpec <*> typeSpec <*> initSpec <* P.eof) (T.unpack str) of
      [((m, t, i),"")] ->
        let moduleName = T.pack <$> m in
        case resolveType' moduleName (T.pack t) Naked (T.pack $ fromMaybe "" i) of
          Right (typ, container, dflt) -> pure $ Field { moduleName, typ, container, dflt }
          Left e -> Left e

      _ -> Left $ "Invalid spcification of field "<>str

  where

    typeSpec :: P.ReadP String
    typeSpec = (:) <$> P.satisfy (\c -> P.isUpper c || P.isLower c)
               <*> P.many (P.satisfy (\c -> P.isUpper c || P.isLower c || P.isDigit c || (c == '_')))

    modSpec :: P.ReadP (Maybe String)
    modSpec = P.option Nothing (Just <$> typeSpec <* P.char '.')

    initSpec :: P.ReadP (Maybe String)
    initSpec = P.option Nothing (Just <$ P.skipSpaces <* P.string "<*>" <* P.skipSpaces <*> P.many1 P.get)

    listSpec :: P.ReadP (Int, Maybe String, String)
    listSpec = (,,,) <$  P.skipSpaces
                     <*> lDim <* P.skipSpaces
                     <*> modSpec <*> typeSpec
                     <*  P.skipSpaces <*> rDim <* P.eof >>= check
      where
        check (k,m,s,k')
          | k == k'   =
            if k == ""
            then pure (1, m, s)
            else pure (read k, m, s)
          | otherwise = fail "Incorrect list definition"
        lDim = P.char '<' *> P.many (P.satisfy P.isDigit) <* P.char '|'
        rDim = P.char '|' *> P.many (P.satisfy P.isDigit) <* P.char '>'

    resolveType' = resolveType search mName'

parseObj :: (T.Text -> T.Text -> SearchResult)
            -> T.Text
            -> Object
            -> Either T.Text Field
parseObj search mName' o =
  let typeField :: Maybe T.Text
      typeField = o ^? at "type" ._Just ._String
      parseObj' = parseObj search mName'
  in
  case typeField of
    Just "list" ->
      let v :: Maybe Value
          v = o ^? at "value" . _Just
      in
      case v of
        Just (String t)  -> case resolveType' Nothing t (Lst 1) "" of
          Right (ty,c,d) -> Right $ Field Nothing ty c d
          Left e         -> Left e
        Just (Object o') ->
          case parseObj' o' of
            Right (Field m t (Lst k) d)     -> Right $ Field m t (Lst $ k + 1) d
            Right (Field _ _  Opt    _)     -> Left  "list of optional values does not make sense"
            Right (Field _ _  (OptLst _) _) -> Left  "list of (optional list) does not make sense"
            Right (Field m t Naked   d)     -> Right $  Field m t (Lst 1) d
            Left e -> Left e
        _ -> Left "[value] field must be a string or the object"
    Just "optional" ->
      case o ^? at "value" . _Just of
        Just (String t)  -> case resolveType' Nothing t Opt "" of
          Right (ty,opt,d) -> Right $ Field Nothing ty opt d
          Left e           -> Left e
        Just (Object o') ->
          case parseObj' o' of
            Right (Field m t (Lst k) d)    -> Right $ Field m t (OptLst k) d
            Right (Field _ _ Opt    _)     -> Left  "optional (optional list) does not make sense"
            Right (Field _ _ (OptLst _) _) -> Left  "optional (optional value) does not make sense"
            Right (Field m t Naked   d)    -> Right $ Field m t Naked d
            Left e -> Left e
        _ -> Left "[value] field must be a string or the object"
    Just "external" ->
      case (o ^? at "module" ._Just ._String,
            o ^? at "value"  ._Just ._String,
            o ^? at "init" ._Just ._String)
      of
        (Just m, Just t, Just d) -> case resolveType' (Just m) t Naked d of
           Right (ty, _, d') -> Right $ Field (Just m) ty Naked d'
           Left e            -> Left e
        (Just m, Just t, Nothing) -> case resolveType' (Just m) t Naked "" of
           Right (ty, _, d') -> Right $ Field (Just m) ty Naked d'
           Left e            -> Left e
        (Just _, Nothing, _)  -> Left "[value] field is not defined with the external module type"
        (Nothing, Just _, _)  -> Left "[module] field is not defined with the external module type"
        (Nothing, Nothing,_)  -> Left "[module] and [value] fields are not defined with the external module type"
    Just t ->
      case o ^? at "init" ._Just of
        Nothing  -> case resolveType' Nothing t Naked "" of
          Right (ty, _, _) -> Right $ Field Nothing ty Naked NoDflt
          Left e           -> Left e
        Just (Bool True)  -> case resolveType' Nothing t Naked "true" of
          Right (ty, _, d) -> Right $ Field Nothing ty Naked d
          Left e           -> Left e
        Just (Bool False)  -> case resolveType' Nothing t Naked "false" of
          Right (ty, _, d) -> Right $ Field Nothing ty Naked d
          Left e           -> Left e
        Just (Number i)   ->
          let d = case floatingOrInteger i :: Either Double Int of
                Left  r  -> T.pack $ show r
                Right i' -> T.pack $ show i'
          in case  resolveType' Nothing t Naked d of
              Right (ty, _, d') -> Right $ Field Nothing ty Naked d'
              Left e            -> Left e
        Just (String s)   -> case resolveType' Nothing t Naked s of
          Right (ty, _, d') -> Right $ Field Nothing ty Naked d'
          Left e            -> Left e
        _  -> Left "[init] field should be a string or a boolean, or a number"

    Nothing -> Left "[type] field must be a string"

    where
      resolveType' = resolveType search mName'

resolveType :: (T.Text -> T.Text -> SearchResult)
            -> T.Text
            -> Maybe T.Text -> T.Text -> Container -> T.Text -> Either T.Text (FType, Container, Dflt)
resolveType search mName'  modName typeCandidate containerCandidate dfltCandidate =
  case (typeCandidate, containerCandidate, dfltCandidate) of
    (_, Lst _,s) | s /= ""  ->
          Left "Providing default parameter to List does not make sense"
    (t,OptLst _,_) | t `elem` specialTypesS  ->
          Left ("Making special type "<>t<>" optional does not make sense")
    (t,c,s) | Just ty <- parseType t ->
       case parseDefaultValue t s of
          Right d -> Right (ty , c, d)
          Left  e -> Left e
    (t,c,s) ->
       let mName'' = fromMaybe mName' modName in
       case search mName'' t of
          Err e -> Left e
          EnumFound  e -> case () of
               _ | T.null s -> Right (Enum t (eDflt e) (fst <$> eValues e), c, NoDflt)
                 | s `elem` (fst <$> eValues e) -> Right (Enum t (eDflt e) (fst <$> eValues e), c, DfltS s)
                 | otherwise ->
                         Left (" default value `"<>s<>"` is not part of the enum `"
                              <>t<>"` in module `"<>mName''<>"`")
          StructFound -> Right (Struct t, c, NoDflt)

specialTypesS :: Set.Set T.Text
specialTypesS = Set.fromList [ "BoolS"
                             , "IntS"
                             , "DoubleS"
                             , "DollarS"
                             , "YearS"
                             , "PhoneS"
                             , "DateS"
                             ]

parseType :: T.Text -> Maybe FType
parseType =
  let m =  M.fromList [ ("BoolS", BoolS)
                      , ("IntS", IntS)
                      , ("DoubleS", DoubleS)
                      , ("DollarS", DollarS)
                      , ("YearS", YearS)
                      , ("PhoneS", PhoneS)
                      , ("DateS", DateS)
                      , ("bool", PrimBool)
                      , ("int", PrimInt)
                      , ("float", PrimDouble)
                      , ("string", PrimString)
                      ]
  in flip M.lookup m

parseDefaultValue :: T.Text -> T.Text -> Either T.Text Dflt
parseDefaultValue _ "" = Right NoDflt
parseDefaultValue ty va =
  let v = T.unpack va in
  case ty of
    "BoolS"   -> case v of
      "true"  -> Right $ DfltB True
      "false" -> Right $ DfltB False
      _       -> Left $ "wrong default value of type 'BoolS': "<>va
    "IntS"    -> case reads v of
      [(i,"")] -> Right $ DfltI i
      _        -> Left $ "wrong default value of type 'IntS': "<>va
    "FloatS"    -> case reads v of
      [(f,"")] -> Right $ DfltD f
      _        -> Left $ "wrong default value of type 'FloatS': "<>va
    "DoubleS"  -> case reads v of
      [(f,"")] -> Right $ DfltD f
      _        -> Left $ "wrong default value of type 'DoubleS': "<>va
    "DollarS"  -> case reads v of
      [(i,"")] -> Right $ DfltI i
      _        -> Left $ "wrong default value of type 'DollarS': "<>va
    "YearS"  -> case reads v of
      [(i,"")] -> Right $ DfltI i
      _        -> Left $ "wrong default value of type 'YearS': "<>va
    "PhoneS"   -> if checkPhone v
      then Right $ DfltS va
      else Left $ "wrong default value of type 'PhoneS': "<>va
    "DateS"  -> case DT.readSTime False DT.defaultTimeLocale "%0m/%0d/%0Y " v of
      [(d,"")] -> Right $ DfltDate d
      _        -> Left $ "wrong default value of type 'DateS': "<>va
    "bool"   -> case v of
      "true"  -> Right $ DfltB True
      "false" -> Right $ DfltB False
      _       -> Left $ "wrong default value of type 'bool: "<>va
    "int"    -> case reads v of
      [(i,"")] -> Right $ DfltI i
      _        -> Left $ "wrong default value of type 'int': "<>va
    "float"    -> case reads v of
      [(f,"")] -> Right $ DfltD f
      _        -> Left $ "wrong default value of type 'float': "<>va
    "double"   -> case reads v of
      [(f,"")] -> Right $ DfltD f
      _        -> Left $ "wrong default value of type 'double': "<>va
    "string"   -> Right $ DfltS va
    _          -> Left $ "Undefined type: "<>ty
  where
    checkPhone "N/A" = True
    checkPhone s =
      case P.readP_to_S pPhoneS s of
        [((),"")] -> True
        _         -> False
      where
        pPhoneS :: P.ReadP ()
        pPhoneS = P.between (P.char '(') (P.char ')') (P.count 3 (P.satisfy P.isDigit))
                 *>  P.skipSpaces *>  P.optional (P.char '-') *>  P.skipSpaces
                 *>  P.count 3 (P.satisfy P.isDigit)
                 *>  P.char '-'
                 *>  P.count 4 (P.satisfy P.isDigit)
                 *>  P.eof
