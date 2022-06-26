{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Import
import           Options.Applicative.Simple
import qualified Paths_Gen


import           RIO.Process

import           Run

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsDTD     :: !FilePath
  , optionsModules :: ![Text]
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
       <*> strArgument (metavar "HDTD")
       <*> many (strArgument (metavar "module"))
    )
    empty
  lo' <- logOptionsHandle stderr (optionsVerbose options)
  let lo = setLogUseColor True $ setLogUseTime True lo'
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { verbose = optionsVerbose options
          , appLogFunc = lf
          , appProcessContext = pc
          }
    in runRIO app $ run (optionsDTD options) (optionsModules options)
