#!/usr/bin/env stack
-- stack --resolver lts-16.7 script

import           System.IO

moduleName :: String
moduleName = "Camelizer.CamelCase.Words"

wordsFile :: String
wordsFile = "google-10000-english-usa.txt"

additionsFile :: String
additionsFile = "additions.txt"

outFile :: FilePath
outFile = "Words.hs"

main :: IO ()
main = do
  a <- words <$> readFile additionsFile
  w' <- words <$> readFile wordsFile
  let w = a <> w'
  withFile outFile WriteMode $ \h -> do
    hPutStrLn h $ "module "<>moduleName<>" where"
    hPutStrLn h ""
    hPutStrLn h "import           RIO"
    -- hPutStrLn h "import           RIO.Text"
    hPutStrLn h "import           RIO.HashMap"
    hPutStrLn h ""
    hPutStrLn h "words :: HashMap Text Double"
    hPutStr   h "words = fromList "
    hPutStrLn h $ show $ dict w
  where
    dict :: [String] -> [(String, Double)]
    dict words = (\(k, w) -> (w, log $ nrm * fromIntegral k)) <$> zip [1..] words

      where
        nrm :: Double
        nrm = (log . fromIntegral . length $! words)
