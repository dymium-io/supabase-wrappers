module Camelizer(camelizerDef) where

import           Import

import qualified RIO.Directory       as D
import qualified RIO.HashMap         as HM
import qualified RIO.List            as Lst
import qualified RIO.Process         as Prc
import qualified RIO.Set             as Set
import qualified RIO.Text            as T

import           Data.Yaml
import           Lens.Micro.Aeson

import           Camelizer.CamelCase

import           Data.Char           (isAlphaNum)
import           GHC.IO.Handle       (hPutStr)

camelizerDef :: Maybe Text -> [ModuleDef] -> RIO App Camelizer
camelizerDef Nothing _ = pure id
camelizerDef (Just mf) mds = do
  existingMap  <- D.doesFileExist (T.unpack mf) >>= \case
    True -> liftIO (decodeAllFileEither . T.unpack $ mf)
    False -> pure $ Right []
  case existingMap of
    Right []  -> getDiff mf HM.empty mds
    Right [m] -> getDiff mf m mds
    Left err -> do
      logError $ displayShow err
      exitFailure
    _ -> do
      logError . display $ ("Wrong format of the mapping file ["<>mf<>"]")
      exitFailure


getDiff :: Text
        -> HM.HashMap Text Text
        -> [ModuleDef]
        -> RIO App (Text -> Text)
getDiff mf m mds = do
  let mdsK = mdsKeys mds
      mK   = Set.fromList $ HM.keys m
      adds = Set.difference mdsK mK
      dels = Set.difference mK mdsK
  if null adds && null dels
    then  pure (\k ->
                 fromMaybe (error $ T.unpack $ "Undefined key "<>k)
                 (k `HM.lookup` m))
    else edit mf (zip (Set.elems adds) (camelCase <$> Set.elems adds))
         (Set.elems dels) >>= \case
    Nothing -> do
      logInfo "As you wish..."
      exitSuccess
    Just (adds', dels') -> do
      let m'  = applyEdits m adds' dels'
          mK' = Set.fromList $ HM.keys m'
      if mK' == mdsK
        then do
        withBinaryFile (T.unpack mf) WriteMode $ writeMap m' ""
        pure (\k ->
               fromMaybe (error $ T.unpack $ "Undefined key "<>k)
               (k `HM.lookup` m'))
        else do
          logError "Something went wrong.."
          exitFailure


edit :: Text
     -> [(Text,Text)]
     -> [Text]
     -> RIO App (Maybe (HM.HashMap Text Text,Set.Set Text))
edit mf adds dels = withSystemTempFile (T.unpack $ T.concatMap (\c -> if isAlphaNum c then T.singleton c else "_")  mf) $ \fn h -> do
  liftIO $ writeHeader h
  unless (null adds) $ do
    liftIO $ hPutStr h "Additions:\n"
    liftIO $ mapM_
      (\(k, v) -> hPutStr h $ "  "<>show k<>": "<>show v<>"\n")
      (Lst.sortOn fst adds)
  unless (null dels) $ do
    liftIO $ hPutStr h "Deletions:\n"
    liftIO $ mapM_ (\k -> hPutStr h $ "  - "<>show k<>"\n") (Lst.sort dels)
  hClose h
  -- proc "nano" ["--ignorercfiles",fn] runProcess_
  Prc.proc "vi" [fn] Prc.runProcess_
  liftIO (decodeAllFileEither fn) >>= \case
    Right [m] ->
      if (m :: Value) ^? key "Action" . _String /= Just "APPLY"
      then pure Nothing
      else
        case (m ^? key "Additions" . _Object, m ^? key "Deletions" . _Array .to toList) of
          (Nothing,Nothing)   ->
             pure Nothing
          (Just add,Nothing)  ->
             pure . Just $ (HM.map (view _String) add, Set.empty)
          (Nothing,Just del)  ->
             pure . Just $ (HM.empty, Set.fromList $ map (view _String) del)
          (Just add,Just del) ->
             pure . Just $ (HM.map (view _String) add, Set.fromList $ map (view _String) del)
    _         -> pure Nothing
  where
    writeHeader h = do
      hPutStr h $ "\
       \# This file contains suggested additions/deletions to "<>T.unpack mf<>"\n"
      hPutStr h "\
       \#\n\
       \# Remove the following line if you don't want the changes to be applied\n\
       \Action: APPLY\n\
       \\n"

applyEdits :: HM.HashMap Text Text
           -> HM.HashMap Text Text
           -> Set.Set Text
           -> HM.HashMap Text Text
applyEdits m adds dels = m'
  where
    m'' = HM.filterWithKey (\k _ -> k `notElem` dels) m
    m'  = HM.union m'' adds


writeMap :: HM.HashMap Text Text -> String -> Handle -> RIO App ()
writeMap hm indent h =
  liftIO $ mapM_ (\(k,v) -> hPutStr h $ indent<>show k<>": "<>show v<>"\n") lst
  where
    lst = Lst.sortOn fst $ HM.toList hm


mdsKeys :: [ModuleDef] -> Set.Set Text
mdsKeys mds =
  Set.fromList $ mconcat $ extractNames <$> mds
  where
    extractNames md =
      mzero
      <> mconcat (fromE <$> enums md)
      <> mconcat (fromS <$> structs md)
    fromE e = [eName e] <> (fst <$> eValues e)
    fromS s = [sName s] <> (fst <$> sFields s)
