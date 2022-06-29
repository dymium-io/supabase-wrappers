module Run
  ( run
  )
where

import           Import

import           RIO.FilePath
import           RIO.Lens
import qualified RIO.Map                as M
import qualified RIO.Set                as Set
import qualified RIO.Text               as T

import           Lens.Micro.Aeson

import           Data.Yaml
import           Text.Pretty.Simple

import qualified Camelizer              as Cmz
import           FromYaml

import           Common                 as C

import qualified LangGenerators.GoLang  as GoLang
import qualified LangGenerators.Haskell as Haskell
import qualified LangGenerators.Js      as Js
import qualified LangGenerators.Python  as Python
import qualified LangGenerators.Ts      as Ts

run :: FilePath -> [Text] -> RIO App ()
run hdtdName installations = do
  res <- liftIO (decodeAllFileEither hdtdName :: IO (Either ParseException [M.Map Text DdtDef]))
  case res of
    Right [hdtds] -> do
      hdtdList <- makeHdtdList hdtds
      forM_ hdtdList $ \(name, hdtd') -> do
        logInfo $ "\x1B[34mRunning \x1B[0;31m" <> display name <> "\x1B[0;34m installation...\x1B[0m"
        hdtdD <- normalizeHdtdPaths hdtdName (ddtModules hdtd')
        let lang = ddtLanguage hdtd'
            installPath = InstallPath (ddtRootPath hdtd')
            rootModule = RootModule (ddtRootModule hdtd')
            mapF = ddtMapFile hdtd'
            r =  hdtdD <&> ddtSource
            d = decodeAllFileEither . (takeDirectory hdtdName </>)
        yy <- liftIO (mapM d r :: IO [Either ParseException [Value]])
        let errs = yy ^..traverse ._Left
        unless (null errs) $ do
            mapM_ (logError . displayShow) errs
            exitFailure
        let yy' = yy ^..traverse . _Right & zip hdtdD
        checkSubmodules yy'
        ddt <- fromYaml yy'
        ask <&> verbose >>= flip when (pPrint ddt)
        camelizer <- Cmz.camelizerDef mapF ddt
        case T.toLower lang of
          "haskell" -> Haskell.run ddt installPath rootModule camelizer
          js | js == "js" || js == "javascript" ->
            Js.run ddt installPath rootModule camelizer
          ts | ts == "ts" || ts == "typescript" ->
            Ts.run      ddt installPath rootModule camelizer
          go | go == "go" || go == "golang" ->
            GoLang.run  ddt installPath rootModule camelizer
          "python"  -> Python.run  ddt installPath rootModule camelizer
          _ -> do
            logError . displayShow $
              "Language ["<>lang<>"is not supported\n" <>
              "Supported languages are: Haskell, JavaScript, TypeScript, Golang, and Python"
            exitFailure
    Left err -> do
      logError $ displayShow err
      exitFailure
    _ -> do
      logError "Multiple sections in HDTD file are not supported"
      exitFailure

    where
      makeHdtdList hdtds =
        if null installations
        then pure $ M.toList hdtds
        else forM installations $ \m -> do
          case M.lookup m hdtds of
            Nothing -> do
              logError $ "Installation "<> display m <>" not defined in HDTD file"
              exitFailure
            Just h -> pure (m,h)

      checkSubmodules :: [(DdtModuleDef, [Value])] -> RIO App ()
      checkSubmodules = mapM_ $ \(hddtModule, parsedValues) ->
        let hddtSubmodules = Set.fromList $ M.keys (ddtMod hddtModule)
            definedModules = Set.fromList $ parsedValues ^.. each . key "module" . _String
        in
         case Set.difference hddtSubmodules definedModules of
           d | null d -> pure ()
           d -> do
             forM_(Set.toList d) $ \d' -> logError $ "\x1B[31mError\x1B[0m: Submodule "<>
               displayShow d'  <> " is not defined in "<>
               displayShow (ddtSource hddtModule)
             exitFailure


normalizeHdtdPaths :: FilePath -> [DdtModuleDef] -> RIO App [DdtModuleDef]
normalizeHdtdPaths hdtdName definitions = do
    traverse updateDdtDef definitions
  where

    updateDdtDef :: DdtModuleDef -> RIO App DdtModuleDef
    updateDdtDef ddtDef = do
      defaultDestination <- case  ddtDestination ddtDef of
        Nothing -> pure Nothing
        Just p  -> C.normalisePath p >>= \case
          p'@('/':_ ) -> do
            logError $ displayShow hdtdName<>
              ": absolute path "<>
              displayShow p'<>
              " is not allowed as the destination"
            exitFailure
          p' -> pure $ Just p'
      ddtMod' <- M.traverseWithKey (updateModDest defaultDestination) (ddtMod ddtDef)
      pure $ ddtDef
        { ddtMod = ddtMod'
        , ddtDestination = defaultDestination
        }

    updateModDest :: Maybe FilePath -> Text -> DdtMod -> RIO App DdtMod
    updateModDest defaultDestination mName' ddtMod' =
      case ddtPath ddtMod' of
        Nothing   -> pure $ ddtMod' { ddtPath = defaultDestination }
        Just "//" -> pure $ ddtMod' { ddtPath = Nothing }
        Just ('/': '/' : p) -> do
          p' <- Just <$> C.normalisePath p
          pure $ ddtMod' { ddtPath = p' }
        Just p@('/' : _) -> do
                   logError $  displayShow hdtdName<>
                     ": absolute path "<>
                     displayShow p<>
                     " is not allowed as the destination of "<>
                     display mName'
                   exitFailure
        Just p -> case defaultDestination of
          Nothing -> do
            p' <- Just <$> normalisePath p
            pure $ ddtMod' { ddtPath = p' }
          Just dp -> do
            p' <- Just <$> normalisePath (dp </> p)
            pure $ ddtMod' { ddtPath = p' }

