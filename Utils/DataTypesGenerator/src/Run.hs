module Run
  ( run
  )
where

import           Import

import           RIO.FilePath
import qualified RIO.HashMap            as HM
import           RIO.Lens
import qualified RIO.Map                as M
import qualified RIO.Text               as T

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
run hddtName modules = do
  res <- liftIO (decodeAllFileEither hddtName :: IO (Either ParseException [M.Map Text DdtDef]))
  case res of
    Right [hddts] -> do
      hddtList <- makeHddtList hddts
      forM_ hddtList $ \(name, hddt') -> do
        logInfo $ "\x1B[34mRunning \x1B[0;31m" <> display name <> "\x1B[0;34m installation...\x1B[0m"
        hddtD <- normalizeHddtPaths hddtName (ddtModules hddt')
        let lang = ddtLanguage hddt'
            installPath = InstallPath (ddtRootPath hddt')
            rootModule = RootModule (ddtRootModule hddt')
            mapF = ddtMapFile hddt'
            r =  hddtD <&> ddtSource
            d = decodeAllFileEither . (takeDirectory hddtName </>)
        yy <- liftIO (mapM d r :: IO [Either ParseException [Value]])
        let errs = yy ^..traverse ._Left
        unless (null errs) $ do
            mapM_ (logError . displayShow) errs
            exitFailure
        ddt <- fromYaml $ yy ^..traverse . _Right & zip hddtD
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
      logError "Multiple sections in HDDT file are not supported"
      exitFailure

    where
      makeHddtList hddts =
        if null modules
        then pure $ M.toList hddts
        else forM modules $ \m -> do
          case M.lookup m hddts of
            Nothing -> do
              logError $ "Installation "<> display m <>" not defined in HDDT file"
              exitFailure
            Just h -> pure (m,h)

normalizeHddtPaths :: FilePath -> [DdtModuleDef] -> RIO App [DdtModuleDef]
normalizeHddtPaths hddtName definitions = do
    traverse updateDdtDef definitions
  where

    updateDdtDef :: DdtModuleDef -> RIO App DdtModuleDef
    updateDdtDef ddtDef = do
      defaultDestination <- case  ddtDestination ddtDef of
        Nothing -> pure Nothing
        Just p  -> C.normalisePath p >>= \case
          p'@('/':_ ) -> do
            logError $ displayShow hddtName<>
              ": absolute path "<>
              displayShow p'<>
              " is not allowed as the destination"
            exitFailure
          p' -> pure $ Just p'
      ddtMod' <- HM.traverseWithKey (updateModDest defaultDestination) (ddtMod ddtDef)
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
                   logError $  displayShow hddtName<>
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

