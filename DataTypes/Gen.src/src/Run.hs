module Run
  ( run
  )
where

import           Import

import           RIO.FilePath
import qualified RIO.HashMap            as HM
import           RIO.Lens
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

run :: Lang -> InstallPath -> RootModule -> Maybe Text -> RIO App ()
run lang installPath rootModule mapF = do
  hddtName <- ask <&> appHddtName
  liftIO (decodeAllFileEither $ T.unpack hddtName :: IO (Either ParseException [Ddt])) >>= \case
    Right [hddt'] -> do
      hddt <- normalizeHddtPaths hddt'
      let hddtD = hddt & ddtDefinitions
          r =  hddtD <&> ddtSource
          d = decodeAllFileEither . (takeDirectory (T.unpack hddtName) </>)
      yy <- liftIO (mapM d r :: IO [Either ParseException [Value]])
      let errs = yy ^..traverse ._Left
      unless (null errs) $ do
          mapM_ (logError . displayShow) errs
          exitFailure
      ddt <- fromYaml $ yy ^..traverse . _Right & zip hddtD
      ask <&> verbose >>= flip when (pPrint ddt)
      camelizer <- Cmz.camelizerDef mapF ddt
      case lang of
        Haskell -> Haskell.run ddt installPath rootModule camelizer
        JS      -> Js.run      ddt installPath rootModule camelizer
        GoLang  -> GoLang.run  ddt installPath rootModule camelizer
        Python  -> Python.run  ddt installPath rootModule camelizer
    Left err -> do
      logError $ displayShow err
      exitFailure
    Right _ -> do
      logError $ displayShow ("Wrong format of ["<>hddtName<>"]: multiple objects")
      exitFailure


normalizeHddtPaths :: Ddt -> RIO App Ddt
normalizeHddtPaths ddt = do
    definitions' <- traverse updateDdtDef definitions
    pure $ ddt { ddtDefinitions = definitions' }
  where
    definitions = ddtDefinitions ddt

    updateDdtDef :: DdtDef -> RIO App DdtDef
    updateDdtDef ddtDef = do
      defaultDestination <- case  ddtDestination ddtDef of
        Nothing -> pure Nothing
        Just p  -> C.normalisePath p >>= \case
          p'@('/':_ ) -> do
            hddtName <- ask <&> appHddtName
            logError $ display hddtName<>
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
                   hddtName <- ask <&> appHddtName
                   logError $  display hddtName<>
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

