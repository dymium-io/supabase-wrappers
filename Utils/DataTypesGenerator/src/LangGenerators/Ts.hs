{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module LangGenerators.Ts where

import           RIO
import           RIO.FilePath      (joinPath, (<.>), (</>))
import           RIO.List          (sort, sortOn)
import qualified RIO.Map           as M
import qualified RIO.Set           as Set
import qualified RIO.Text          as T

import           Data.Char         (isUpper)
import qualified Data.Time         as DT
import           NeatInterpolation

import qualified Common            as C

import           Types

run :: [ModuleDef] -> InstallPath -> RootModule -> Camelizer -> RIO App ()
run mDefs installPath@(InstallPath installPath') _rootModule camelizer = do
    logInfo $ "Installing TypeScript modules to " <> displayShow installPath' <> "..."
    C.createInstallPaths installPath mDefs transformPath
    mapM_ (genModule installPath nameMappers) mDefs
  where
    nameMappers = genMappers mDefs camelizer

data NameMappers =
  NameMappers
  { nm_modulesPaths     :: !(Text -> (Text, [FilePath]))
  , nm_mNameMapper      :: !(Text -> Text)
  , nm_enumNameMapper   :: !(Text -> (Text -> Text))
  , nm_enumFldMapper    :: !(Text -> ((Text,Text) -> Text))
  , nm_structNameMapper :: !(Text -> (Text -> Text))
  , nm_structFldMapper  :: !(Text -> ((Text, Text) -> Text))
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
    writeFileUtf8 inst [untrimming|// this file is automatically generated.
  // !!! DO NOT EDIT !!!

  import _ from 'lodash'

  ${imports}

  export let dirtyFlag = false
  function doAlert(s) { console.log(s) }

  ${enumDefs}
  ${structDefs}
  ${readers}

  let setDirtyFlag = () => { dirtyFlag = true }
  let [disableDF,enableDF] = (() => {
      let n = 0
      return [() => {
        if(n === 0) {
          setDirtyFlag = () => {}
          n = 1
        } else {
          n += 1
        }
      },
      () => {
        if(n === 1) {
          setDirtyFlag = () => { dirtyFlag = true }
          n = 0
        } else {
          n -= 1
        }
      }]
  })()|]
  where
    inst =
      let (fName', fPath) = mPath mDef
          fName = T.unpack fName' <.> "ts"
      in
      joinPath (installPath : fPath) </> fName

    imports = mconcat $ importString <$> externalModules mDef
      where
        importString em = T.concat ["import * as ",lName," from ",jsString rPath,"\n"]
          where
            lName = mNameMapper em
            rPath = T.pack . (</> (T.unpack (fst $ modulesPaths em) <.> "ts")) .joinPath $
              case C.calcRelativePath (snd . modulesPaths $ mName') (snd $ modulesPaths em) of
                r@("..":_) -> r
                r          -> "." : r

    mNameMapper = nm_mNameMapper nameMappers
    modulesPaths = nm_modulesPaths nameMappers

    enums' = sortOn eName $ enums mDef
    structs' = sortOn sName $ structs mDef

    mName' = mName mDef

    enumDefs = T.concat $ enumDef nameMappers mName' <$> enums'
    structDefs = T.concat $ structDef nameMappers mName' <$> structs'

    readers = T.concat $ (readerDef <$> (sort . Set.toList) tSet)
                       <> (arrReaderDef <$> [1 .. maxLst])
                       <> [ enumReaderDef | enumEncountered ]
      where
        (tSet, maxLst, enumEncountered) = extracted
        extracted = foldl' extract (Set.empty, 0, False) structs'
        extract s = foldl' getT s . sFields
        getT (tS, mL, wasEnum) (_, Field { typ, container }) =
          case (container, typ) of
            (Lst k, Struct _) -> (tS, max k mL, wasEnum)
            (Lst k, Enum {})  -> (tS, max k mL, True)
            (Lst k, t)        -> (Set.insert t tS, max k mL, wasEnum)
            (    _, Struct _) -> (tS, mL, wasEnum)
            (    _, Enum {})  -> (tS, mL, True)
            (    _, t)        -> (Set.insert t tS, mL, wasEnum)


enumDef :: NameMappers -> T.Text -> EnumDef -> T.Text
enumDef nameMappers mName' eDef = [untrimming|export type ${eName'} =
    | ${eFlds}

  ${humanReadable}
  |]
  where
    en = eName eDef

    eName' = nm_enumNameMapper nameMappers mName' en

    eFlds = T.intercalate "\n| " $ jsString . fst <$> eValues eDef

    humanReadable =
      case () of
        _ | any isJust (snd <$> eValues eDef) -> [untrimming|export function humanReadable${eName'}(__a__ : ${eName'}) : string {
         switch(__a__) {
           ${eCases}
         }
         return '';
       }|]
          | otherwise -> ""

    eCases = T.concat $
       (\(v,d) -> let (v',d') = (jsString v,jsString $ fromMaybe v d) in
                  [untrimming|case ${v'}: return ${d'};|]) <$>
       eValues eDef

structDef :: NameMappers -> T.Text -> StructDef -> T.Text
structDef nameMappers mName' sDef = [untrimming|export class ${csn} {
    ${fldDclrs}

    constructor() {
      this${fldDefs}
    }
    ${gettersAndSetters}

    toJson(): string { return JSON.stringify(this).split('"_').join('"') }

    static fromJson(__a__: any): ${csn} {
      disableDF()
      let cls = new ${csn}()
      if(typeof __a__ === 'object' && __a__ != null) {
         ${fromJson}
      } else {
         doAlert(`${csn}: an attempt to initialize from $${__a__}`)
      }
      enableDF()
      return cls
    }
  }
  |]
  where
    sn = sName sDef
    csn = nm_structNameMapper nameMappers mName' sn
    fldRepr' select = fldRepr select nameMappers mName' sn

    fldDclrs = mconcat $ fldDclr' <$> sFields sDef
      where
        fldDclr' namefield =
          let (jfn', jtyp') = fldDclr nameMappers mName' namefield in
          [untrimming|private ${jfn'}: ${jtyp'}|]

    fldDefs = T.intercalate "this" $ fldRepr' RDef <$> sFields sDef
    gettersAndSetters = mconcat $ fldRepr' RGetSet <$> sFields sDef
    fromJson = mconcat $ fldRepr' RJson <$> sFields sDef

data ReprSelector = RDef | RGetSet | RJson

fldDclr :: NameMappers -> T.Text -> (T.Text, Field) -> (T.Text, T.Text)
fldDclr nameMappers mName' (fn,Field { moduleName, typ, container }) = (jfn', jtyp')
   where
     jfn' = jsString ("_"<>fn)
     jtyp' = case container of
       Lst k    -> arrTyp k
       Opt      -> nakedTyp <> " | null"
       OptLst k -> arrTyp k <> " | null"
       Naked    -> nakedTyp

     arrTyp k = if k == 0 then nakedTyp else "Array<"<>arrTyp (k-1)<>">"

     nakedTyp = case typ of
        BoolS       -> "boolean | null"
        IntS        -> "number | null"
        DoubleS     -> "number | null"
        DollarS     -> "number | null"
        YearS       -> "string | null"
        PhoneS      -> "string | null"
        DateS       -> "string | null"
        PrimBool    -> "boolean"
        PrimInt     -> "number"
        PrimDouble  -> "number"
        PrimString  -> "string"
        Enum en _ _ -> case moduleName of
          Nothing -> nm_enumNameMapper nameMappers mName' en
          Just mn -> nm_mNameMapper nameMappers mn<>"."<>nm_enumNameMapper nameMappers mn en
        Struct stn  -> case moduleName of
          Nothing -> nm_structNameMapper nameMappers mName' stn
          Just mn -> nm_mNameMapper nameMappers mn<>"."<>nm_structNameMapper nameMappers mn stn

fldRepr :: ReprSelector -> NameMappers -> T.Text -> T.Text -> (T.Text,Field) -> T.Text
fldRepr select nameMappers mName' sn namefield@(fn,fld@Field { moduleName, typ, container, dflt }) =
  case select of
    RGetSet ->
      let (_,jtyp') = fldDclr nameMappers mName' namefield in
      [untrimming|get ${sfn}(): ${jtyp'} { return this[${jfn'}] }
      set ${sfn}(__a__: any) {
        ${setter}
      }|]
    RDef  ->    [untrimming|[${jfn'}] = ${dflt'}|]
    RJson -> case container of
      Lst k    -> arr k
      Opt      -> [untrimming|cls.${sfn} = __a__[$jfn] == null ? null : ${naked'}|]
      OptLst k -> optArr k
      Naked    -> [untrimming|cls.${sfn} = ${naked'}|]
  where

    sfn = T.map (\case; '-' -> '_'; ch -> ch) $ nm_structFldMapper nameMappers mName' (sn, fn)
    jfn = jsString fn
    jfn' = jsString ("_"<>fn)

    c reader arg = checkAndSet jfn' $ reader<>"("<>arg<>")"
    r reader arg = reader<>"("<>arg<>")"
    enumReader a d = checkAndSet jfn' $ T.concat ["enumReader(['",T.intercalate "','" a,"'],",d,")"]
    arr k =  [untrimming|cls.${sfn} = array${ks}Reader($naked)(__a__[$jfn])|]
      where ks = T.pack $ show k
    optArr k = [untrimming|cls.$sfn = __a__[$jfn] == null ? null : array${ks}Reader($naked)(__a__[$jfn])|]
      where ks = T.pack $ show k

    args = T.intercalate "','"

    setter = case container of
      Lst _    -> simpleSetter jfn'
      Opt      -> optSetter jfn' naked
      OptLst _ -> simpleSetter jfn'
      Naked    -> naked
    dflt' = case container of
      Lst _    -> "[]"
      Opt      -> jsNull
      OptLst _ -> jsNull
      Naked    -> naked
    naked' = case typ of
      Struct stn -> case moduleName of
        Nothing -> T.concat [ nm_structNameMapper nameMappers mName' stn,".fromJson(__a__[",jfn,"])"]
        Just mn -> T.concat [ nm_mNameMapper nameMappers mn,"."
                            , nm_structNameMapper nameMappers mn stn
                            , ".fromJson(__a__[",jfn,"])"
                            ]
      _A        -> "__a__["<>jfn<>"]"
    naked = case (typ,dflt) of
      (BoolS,NoDflt) ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "BoolSReader"   jsEmptyString
          RJson   -> r "BoolSReader"   jsEmptyString
      (IntS,NoDflt)  ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "IntSReader"    jsEmptyString
          RJson   -> r "IntSReader"    jsEmptyString
      (DoubleS,NoDflt) ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "FloatSReader"  jsEmptyString
          RJson   -> r "FloatSReader"  jsEmptyString
      (DollarS,NoDflt) ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "DollarSReader" jsEmptyString
          RJson   -> r "DollarSReader" jsEmptyString
      (YearS,NoDflt) ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "YearSReader"   jsEmptyString
          RJson   -> r "YearSReader"   jsEmptyString
      (PhoneS,NoDflt) ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "PhoneSReader"  jsEmptyString
          RJson   -> r "PhoneSReader"  jsEmptyString
      (DateS,NoDflt) ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "DateSReader"   jsEmptyString
          RJson   -> r "DateSReader"   jsEmptyString

      (PrimBool,NoDflt) ->
        case select of
          RDef    ->                   "false"
          RGetSet -> c "boolReader"    "false"
          RJson   -> r "boolReader"    "false"
      (PrimInt,NoDflt) ->
        case select of
          RDef    ->                   "0"
          RGetSet -> c "intReader"     "0"
          RJson   -> r "intReader"     "0"
      (PrimDouble,NoDflt) ->
        case select of
          RDef    ->                   "0.0"
          RGetSet -> c "floatReader"   "0.0"
          RJson   -> r "floatReader"   "0.0"
      (PrimString,NoDflt) ->
        case select of
          RDef    ->                   jsEmptyString
          RGetSet -> c "stringReader"  jsEmptyString
          RJson   -> r "stringReader"  jsEmptyString

      (BoolS,DfltB True) ->
        case select of
          RDef    ->                     jsString "true"
          RGetSet -> c "BoolSReader"   $ jsString "true"
          RJson   -> r "BoolSReader"   $ jsString "false"
      (BoolS,DfltB False) ->
        case select of
          RDef    ->                     jsString "false"
          RGetSet -> c "BoolSReader"   $ jsString "false"
          RJson   -> r "BoolSReader"   $ jsString "false"
      (IntS,DfltI i) ->
        case select of
          RDef    ->                     jsShow i
          RGetSet -> c "IntSReader"    $ jsShow i
          RJson   -> r "IntSReader"      jsEmptyString
      (DoubleS,DfltD d) ->
        case select of
          RDef    ->                     jsShow d
          RGetSet -> c "FloatSReader"  $ jsShow d
          RJson   -> r "FloatSReader"    jsEmptyString
      (DollarS,DfltI i) ->
        case select of
          RDef    ->                     jsShow i
          RGetSet -> c "DollarSReader" $ jsShow i
          RJson   -> r "DollarSReader"   jsEmptyString
      (YearS,DfltI i) ->
        case select of
          RDef    ->                     jsShow i
          RGetSet -> c "YearSReader"   $ jsShow i
          RJson   -> r "YearSReader"     jsEmptyString
      (PhoneS,DfltS s) ->
        case select of
          RDef    ->                     jsString s
          RGetSet -> c "PhoneSReader"  $ jsString s
          RJson   -> r "PhoneSReader"    jsEmptyString
      (DateS,DfltDate d) ->
        case select of
          RDef    ->                     jsDate d
          RGetSet -> c "DateSReader"   $ jsDate d
          RJson   -> r "DateSReader"     jsEmptyString

      (PrimBool,DfltB True) ->
        case select of
          RDef    ->                   "true"
          RGetSet -> c "boolReader"    "true"
          RJson   -> r "boolReader"    "false"
      (PrimBool,DfltB False) ->
        case select of
          RDef    ->                   "false"
          RGetSet -> c "boolReader"    "false"
          RJson   -> r "boolReader"    "false"
      (PrimInt,DfltI i) ->
        case select of
          RDef    ->                    T.pack (show i)
          RGetSet -> c "intReader"    $ T.pack (show i)
          RJson   -> r "intReader"      "0"
      (PrimDouble,DfltD d) ->
        case select of
          RDef    ->                     T.pack (show d)
          RGetSet -> c "floatReader"   $ T.pack (show d)
          RJson   -> r "floatReader"     "0.0"
      (PrimString,DfltS s) ->
        case select of
          RDef    ->                     jsShow s
          RGetSet -> c "stringReader"  $ jsShow s
          RJson   -> r "stringReader"    jsEmptyString

      (Enum _ Nothing v, NoDflt) ->
        case select of
          RDef    ->                       jsEmptyString
          RGetSet -> enumReader   ("":v)   jsEmptyString
          RJson   -> T.concat ["enumReader(['",args ("":v),"'],'')"]
      (Enum _ (Just d) v, NoDflt) ->
        case select of
          RDef    ->                       jsString d
          RGetSet -> enumReader        v $ jsString d
          RJson   -> T.concat ["enumReader(['",args v,"'],",jsString d,")"]
      (Enum _ Nothing v, DfltS d) ->
        case select of
          RDef    ->                       jsString d
          RGetSet -> enumReader   ("":v) $ jsString d
          RJson   -> T.concat ["enumReader(['",args ("":v),"'],'')"]
      (Enum _ (Just _) v, DfltS d) ->
        case select of
          RDef    ->                       jsString d
          RGetSet -> enumReader        v $ jsString d
          RJson   -> T.concat ["enumReader(['",args v,"'],",jsString d,")"]

      (Struct stn,NoDflt) ->
        case select of
          RDef    -> case moduleName of
            Nothing -> "new "<>nm_structNameMapper nameMappers mName' stn<>"()"
            Just mn -> "new "<>nm_mNameMapper nameMappers mn<>"."
                             <>nm_structNameMapper nameMappers mn stn<>"()"
          RGetSet -> simpleSetter jfn'
          RJson -> case moduleName of
            Nothing -> nm_structNameMapper nameMappers mName' stn<>".fromJson"
            Just mn -> nm_mNameMapper nameMappers mn<>"."
                       <>nm_structNameMapper nameMappers mn stn<>".fromJson"

      _ERR -> error $ "Invalid spec in module "<>show mName'<>": " <> show (fn,fld)


simpleSetter :: T.Text -> T.Text
simpleSetter jfn = [untrimming|setDirtyFlag()
  this[$jfn] = __a__|]

optSetter :: T.Text -> T.Text -> T.Text
optSetter jfn  reader = [untrimming|if(__a__ == null) {
    if(this[$jfn] == null) { return }
    setDirtyFlag()
    this[$jfn] = null
    return
  } else {
    $reader
  }|]

checkAndSet :: T.Text -> T.Text -> T.Text
checkAndSet jfn reader = [untrimming|let __v__ = $reader(__a__)
  if(!_.isEqual(__v__,this[$jfn])) {
    setDirtyFlag()
    this[$jfn] = __v__
  }|]

checkAndSetEnum :: T.Text -> T.Text -> T.Text -> T.Text
checkAndSetEnum jfn v d = [untrimming|let v = enumReader(__a__,${v}, ${d})
  if(v !== this[$jfn]) {
    setDirtyFlag()
    this[$jfn] = v
  }|]

readerDef :: FType -> T.Text
readerDef = \case
  PrimString -> [untrimming|function stringReader(__dflt__) {
    return ((__a__) => {
      if(_.isString(__a__)) {
        return __a__
      }
      doAlert(`stringReader: $${__a__} is not a string`)
      return __dflt__
    })
  }|]
  PrimBool -> [untrimming|function boolReader(__dflt__) {
    return ((__a__) => {
      if(_.isBoolean(__a__)) {
        return __a__
      }
      doAlert(`boolReader: $${__a__} is not a boolean`)
      if(__a__ === "yes" || __a__ === "true") {
        return true
      }
      if(__a__ === "no" || __a__ === "false") {
        return false
      }
      return __dflt__
    })
  }|]

  PrimInt -> [untrimming|function intReader(__dflt__) {
    return ((__a__) => {
      if(_.isInteger(__a__)) {
        return __a__
      }
      doAlert(`intReader: $${__a__} is not an integer`)
      if(_.isString(__a__)) {
        let v = parseInt(__a__)
        if(_.isFinite(v)) {
          return v
        }
      }
      return __dflt__
    })
  }|]

  PrimDouble -> [untrimming|function floatReader(__dflt__) {
    return ((__a__) => {
      if(_.isFinite(__a__)) {
        return __a__
      }
      doAlert(`floatReader: $${__a__} is not a number`)
      if(_.isString(__a__)) {
        let v = parseFloat(__a__)
        if(_.isFinite(v)) {
          return v
        }
      }
      return __dflt__
    })
  }|]

  BoolS -> [untrimming|const BoolSReader = (() =>
    ((__dflt__) =>
      ((__a__) => {
        if(_.isString(__a__) && (__a__ === 'true' || __a__ === 'false' || __a__ === '')) {
          return __a__
        }
        doAlert(`BoolSReader: $${__a__} is not a BoolS`)
        if(__a__ === true) { return 'true' }
        if(__a__ === false) { return 'false' }
        return __dflt__
      })))()|]

  IntS -> [untrimming|const IntSReader = (() => {
    const r = RegExp('^\\d*$$')
    return ((__dflt__) =>
      ((__a__) => {
        if(_.isString(__a__) && r.test(__a__)) {
          return __a__
        }
        doAlert(`IntSReader: $${__a__} is not an IntS`)
        if(_.isInteger(__a__)) {
          return __a__.toString()
        }
        return __dflt__
      })
    )
  })()|]

  DoubleS -> [untrimming|const FloatSReader = (() => {
    const r = RegExp('^[-+]?\\d+[.]?\\d*([eE][-+]?\\d+)?$$')
    return ((__dflt__) =>
      ((__a__) => {
        if(_.isString(__a__) && (__a__ === '' || r.test(__a__))) {
          return __a__
        }
        doAlert(`FloatSReader: $${__a__} is not a FloatS`)
        if(_.isFinite(__a__)) {
          return __a__.toString()
        }
        return __dflt__
      })
    )
  })()|]

  DollarS -> [untrimming|const DollarSReader = (() => {
    const r = RegExp('^\\d*$$')
    return ((__dflt__) =>
      ((__a__) => {
        if(_.isString(__a__) && r.test(__a__)) {
          return __a__
        }
        doAlert(`DollarSReader: $${__a__} is not a DollarS`)
        if(_.isInteger(__a__)) {
          return __a__.toString()
        }
        return __dflt__
      })
    )
  })()|]

  YearS -> [untrimming|const YearSReader = (() => {
    const r = RegExp('^\\d*$$')
    return ((__dflt__) =>
      ((__a__) => {
        if(_.isString(__a__) && r.test(__a__)) {
          return __a__
        }
        doAlert(`YearSReader: $${__a__} is not a YearS`)
        if(_.isInteger(__a__)) {
          return __a__.toString()
        }
        return __dflt__
      })
    )
  })()|]

  DateS -> [untrimming|const DateSReader = (() => {
    // const r = RegExp('^\\d{2}/\\d{2}/\\d{4}$$')
    return ((__dflt__) =>
      ((__a__) => {
        if(_.isString(__a__)) {
          return __a__
        }
        doAlert(`DateSReader: $${__a__} is not a DateS`)
        return __dflt__
      })
    )
  })()|]

  PhoneS -> [untrimming|const PhoneSReader = (() => {
    // const r = RegExp('^\\(\\d{3}\\)-\\d{3}-\\d{4}$$')
    return ((__dflt__) =>
      ((__a__) => {
        if(_.isString(__a__)) {
          return __a__
        }
        doAlert(`PhoneSReader: $${__a__} is not a PhoneS`)
        return __dflt__
      })
    )
  })()|]

  err -> error ("-- IMPOSSIBLE: "<>show err)

enumReaderDef :: T.Text
enumReaderDef = [untrimming|function enumReader(__v__,__dflt__) {
    return ((__a__) => {
      if(!_.isString(__a__)) {
        doAlert(`enumReader: $${__a__} is not a string`)
        return __dflt__
      }
      if(__a__ !== __dflt__ && !_.includes(__v__,__a__)) {
        doAlert(`enumReader: $${__a__} is not in $${__v__}`)
        return __dflt__
      }
      return __a__
    })
  }|]

arrReaderDef :: Int -> T.Text
arrReaderDef 1 = [untrimming|function array1Reader(__r__) {
    return ((__a__) => {
      if(!_.isArray(__a__)) {
        doAlert(`arrayReader: $${__a__} is not an array`)
        return []
      }
      return __a__.map(__r__)
    })
  }|]

arrReaderDef k = [untrimming|function array${ks}Reader(__r__) {
    return ((__a__) => {
      if(!_.isArray(__a__)) {
        doAlert(`arrayReader: $${__a__} is not an array`)
        return []
      }
      return __a__.map(array${ks1}Reader(__r__))
    })
  }|]
  where
    ks = T.pack $ show k
    ks1 = T.pack (show $ k-1)

jsString :: T.Text -> T.Text
jsString = T.cons '\'' . flip T.snoc '\''

jsShow :: Show a => a -> T.Text
jsShow = jsString . T.pack . show

jsEmptyString :: T.Text
jsEmptyString = "''"

jsNull :: T.Text
jsNull = "null"

jsDate :: DT.Day -> T.Text
jsDate = jsString . T.pack . DT.formatTime DT.defaultTimeLocale "%0m/%0d/%0Y"

genMappers :: [ModuleDef] -> Camelizer -> NameMappers
genMappers mDefs camelizer =
  NameMappers {
    nm_modulesPaths,
    nm_mNameMapper = C.hashMapper mDefs,
    nm_enumNameMapper,
    nm_structNameMapper,
    nm_enumFldMapper =
      let m = M.fromList $ (mName &&& eFldMapper) <$> mDefs in
      \mn -> fromMaybe (error "-- IMPOSSIBLE --") $ mn `M.lookup` m,
    nm_structFldMapper = const snd
    }
  where

    nm_modulesPaths :: T.Text -> (T.Text, [FilePath])
    nm_modulesPaths =
      let
        m = M.fromList $ (mName &&& mPath) <$> mDefs
      in
      \mn -> fromMaybe (error $ "can not find path to module `"<>T.unpack mn<>"`")
             (mn `M.lookup` m)

    nm_enumNameMapper :: T.Text -> (T.Text -> T.Text)
    nm_enumNameMapper =
      let m = M.fromList $ (mName &&& eNameMapper) <$> mDefs in
      \mn -> fromMaybe (error "-- IMPOSSIBLE --") $ mn `M.lookup` m

    nm_structNameMapper =
      let m = M.fromList $ (mName &&& sNameMapper) <$> mDefs in
      \mn -> fromMaybe (error "-- IMPOSSIBLE --") $ mn `M.lookup` m


    eFldMapper :: ModuleDef -> ((Text, Text) -> Text)
    eFldMapper mDef =
      let m = C.uniqNames (eShortNameGen mDef) (eLongNameGen mDef) $
              mconcat $
              uncurry C.enPairing . (eName &&& (fst <$>) . eValues)
              <$> enums mDef
      in (\e -> fromMaybe (error "-- IMPOSSIBLE --") $ e `M.lookup` m)

    eShortNameGen :: ModuleDef -> (Text,Text) -> Text
    eShortNameGen mDef (eName', fName') =
      let en = nm_enumNameMapper (mName mDef) eName'
          prefixCandidate = T.filter isUpper en
          prefix = case () of
            _ | T.null prefixCandidate -> T.toUpper $ T.take 1 en
              | otherwise              -> prefixCandidate
      in
      prefix<>"_"<>sanitize fName'

    eLongNameGen :: ModuleDef -> [(Text,Text)] -> [Text]
    eLongNameGen _mDef =
      ((\(eName', fName') -> eName'<>"_"<>sanitize fName') <$>)

    sNameMapper :: ModuleDef -> (Text -> Text)
    sNameMapper mDef =
      let m = M.fromList $ (sName &&& C.capitalizeT . camelizer . sName) <$> structs mDef in
      (\sn -> fromMaybe (error "-- IMPOSSIBLE --") $ sn `M.lookup` m)

    eNameMapper :: ModuleDef -> (Text -> Text)
    eNameMapper mDef =
      let m = M.fromList $ (eName &&& C.capitalizeT . camelizer . eName) <$> enums mDef in
      (\en -> fromMaybe (error $ "-- IMPOSSIBLE --`"<>T.unpack en<>"`") $ en `M.lookup` m)

    sanitize = sanitizeEnumFld camelizer


sanitizeEnumFld :: Camelizer -> Text -> Text
sanitizeEnumFld camelizer ef =
  T.map (\case
            '-' -> '_'
            '*' -> '_'
            '!' -> '_'
            c   -> c)
  $ (C.capitalizeT . camelizer) ef

transformPath :: FilePath -> FilePath
transformPath = id
