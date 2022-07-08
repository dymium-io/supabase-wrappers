module Common where

import           Data.Char     (isLower, isUpper, toLower, toUpper)
import           Data.Hashable (hash)
import           Data.Tuple    (swap)


import qualified RIO.Directory as D
import           RIO.FilePath  (joinPath, normalise, splitDirectories, (</>))
import           RIO.Lens      (each)
import qualified RIO.Map       as M
import qualified RIO.Set       as Set
import qualified RIO.Text      as T

import           Import

createInstallPaths :: InstallPath ->
                      [ModuleDef] ->
                      (FilePath -> FilePath) ->
                      RIO App ()
createInstallPaths (InstallPath installPath) mDef transformPath = do
  paths <- traverse (normalisePath . joinPath)
           $ mDef ^..each .to (snd . mPath)
  let uniqPaths = Set.elems $ Set.fromList paths
  createDirectoryIfMissing installPath
  mapM_ (createDirectoryIfMissing . (installPath </>) . transformPath) uniqPaths
    where
      createDirectoryIfMissing d = do
        dExists <- D.doesDirectoryExist d
        unless dExists $ do
          logInfo $ "Creating directory "<>displayShow d
          D.createDirectoryIfMissing True d


capitalize :: String -> String
capitalize = \case
  f : rest | isLower f -> toUpper f : rest
  s                    -> s

lowerize :: String -> String
lowerize = \case
  f : rest | isUpper f -> toLower f : rest
  s                    -> s

capitalizeT :: Text -> Text
capitalizeT s
  | Just (c, rest) <- T.uncons s, isLower c = T.cons (toUpper c) rest
  | otherwise = s

lowerizeT :: Text -> Text
lowerizeT s
  | Just (c, rest) <- T.uncons s, isUpper c = T.cons (toLower c) rest
  | otherwise = s

enPairing :: Functor f => a -> f b -> f (a,b)
enPairing a b = (a,) <$> b

uniqNames :: (Ord c) =>
             ( c -> Text ) ->
             ( [c] -> [Text] ) ->
             [ c ] ->
             M.Map c Text
uniqNames shortNameGen longNameGen lst =
  let
    hm' = M.fromListWith merge $ (shortNameGen &&& Right) <$> lst
    merge = curry $ \case
      (Right r1, Right r2) -> Left [r1,r2]
      (Right r,  Left  rl) -> Left (r : rl)
      (Left  rl, Right r)  -> Left (r : rl)
      (Left  r1, Left  r2) -> Left (r1 <> r2)
  in
    M.fromList $ mconcat $
    (\case
        (Right n, v) -> [(n,v)]
        (Left  n, _) -> zip n $ longNameGen n
    )  . swap <$> M.toList hm'

checkedUniqNames :: (Ord c) =>
             ( c -> Text ) ->
             [ c ] ->
             Either [(Text, [c])] (M.Map c Text)
checkedUniqNames shortNameGen lst =
  let
    hm' = M.fromListWith merge $ (shortNameGen &&& Right) <$> lst
    merge = curry $ \case
      (Right r1, Right r2) -> Left [r1,r2]
      (Right r,  Left  rl) -> Left (r : rl)
      (Left  rl, Right r)  -> Left (r : rl)
      (Left  r1, Left  r2) -> Left (r1 <> r2)
  in
   case () of
     _ | null $ lefts (M.elems hm') ->
         Right $ M.fromList [ (fromRight (error "* IMPOSSIBLE *") n, v) | (v,n) <- M.toList hm' ]
       | otherwise ->
         Left $ [ (v, fromLeft (error "* IMPOSSIBLE *") n) | (v,n) <- M.toList hm', isLeft n ]

hashMapper :: [ModuleDef] -> (T.Text -> T.Text)
hashMapper mDefs =
  let m = M.fromList $
          (\(mn, p) -> (mn, mn<>"_"<>transform p)) . (mName &&& mPath) <$> mDefs
      transform = T.pack . take 8 . drop 1 . show . hash
  in
   \mn -> fromMaybe (error $ "mNameMapper: module `"<>T.unpack mn<>"` not found")
          (mn `M.lookup` m)


normalisePath :: FilePath -> RIO App FilePath
normalisePath fp = joinPath <$> go [] (splitDirectories $ normalise fp)
  where
      go bld [] = pure $ reverse bld
      go [] (".." : _) = do
                logError $ "Wrong relative path "<>displayShow fp
                exitFailure
      go (_ : bld) (".." : rest) = go bld rest
      go bld (x : rest) = go (x : bld) rest

calcRelativePath :: [FilePath] -> [FilePath] -> [FilePath]
calcRelativePath origin target = (".." <$ origin') <> target'
  where
    (origin',target') = aux origin target

    aux :: [FilePath] -> [FilePath] -> ([FilePath], [FilePath])
    aux [] e  = ([], e)
    aux c []  = (c, [])
    aux c@(hc:tc) e@(he:te)
      | hc == he  = aux tc te
      | otherwise = (c,e)

