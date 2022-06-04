{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Import
import           Options.Applicative.Simple
import qualified Paths_Gen


import           RIO.List                   (intercalate)
import           RIO.Process
import qualified RIO.Text                   as T

import           Run

-- | Command line arguments
data Options = Options
  { optionsVerbose  :: !Bool
  , optionsPath     :: !Text
  , optionsLanguage :: !Lang
  , optionsRoot     :: !(Maybe Text)
  , optionsMapF     :: !(Maybe Text)
  , optionsDTD      :: !Text
  }

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_Gen.version)
    "Generate type definitions from Hierarchical Dymium Types Definition file"
    ""
    (Options
       <$> switch ( long "verbose"
                    <> short 'v'
                    <> help "Verbose output?"
                  )
       <*> strOption ( long "path"
                       <> short 'p'
                       <> metavar "PATH"
                       <> help "Installation path"
                     )
       <*> option auto ( long "language"
                         <> short 'l'
                         <> metavar ("<"<>intercalate "|" ( show <$> [minBound..maxBound :: Lang])<>">")
                         <> help "Programming language"
                       )
       <*> option (Just <$> str) ( long "root"
                                   <> short 'm'
                                   <> short 'r'
                                   <> metavar "MODULE"
                                   <> help "Root module (optional)"
                                   <> value Nothing
                                 )
       <*> option (Just <$> str) ( long "map-file"
                                   <> metavar "MAP-FILE"
                                   <> help "Additional mapping from DTD to type values"
                                   <> value Nothing
                                 )
       <*> strArgument (metavar "HDTD")
    )
    empty
  lo' <- logOptionsHandle stderr (optionsVerbose options)
  let lo = setLogUseColor True $ setLogUseTime True lo'
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { verbose = optionsVerbose options
          , appHddtName = optionsDTD options
          , appLogFunc = lf
          , appProcessContext = pc
          }
    in runRIO app $
       run (optionsLanguage options)
           (InstallPath . T.unpack $ optionsPath options)
           (RootModule $ optionsRoot options)
           (optionsMapF options)
