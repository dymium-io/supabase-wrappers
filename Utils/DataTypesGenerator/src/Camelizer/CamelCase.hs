module Camelizer.CamelCase where

import           RIO
import           RIO.List.Partial          ((!!))
import qualified RIO.Map                   as M
import qualified RIO.Text                  as T

import qualified Camelizer.CamelCase.Words

type Cost = (Int,Double)

camelCase :: Text -> Text
camelCase str = T.concat $ T.toTitle <$> extract len []
  where
    s = T.toLower str

    extract 0 lst = lst
    extract l lst =
      extract l' $ sub l' l : lst
      where l' = l - snd (best !! l)

    best :: [(Cost,Int)]
    best = ((0,0),0) : resolve 1

    resolve :: Int -> [(Cost, Int)]
    resolve k =
      if k > len
      then []
      else
        findMin ((\(b,l) -> (cost l k `add` fst b, k - l)) <$> zip best [0 .. k-1])
        : resolve (k + 1)

    add :: (Int, Double) -> (Int, Double) -> (Int, Double)
    add (k, c) (k', c') = (k + k', c + c')

    sub :: Int -> Int -> Text
    sub l k = T.take (k - l) . T.drop l $ s

    cost :: Int -> Int -> Cost
    cost l k = case flip M.lookup Camelizer.CamelCase.Words.words $ sub l k of
      Nothing -> (k - l, 0)
      Just c  -> (0, c)

    findMin :: [(Cost, Int)] -> (Cost, Int)
    findMin [] = error "-- impossible --"
    findMin [cl] = cl
    findMin (cl@((w,c),_) : rest) = minimum $ findMin rest
      where
        minimum cl'@((w',c'),_)
          | w' < w = cl'
          | w' > w = cl
          | c' < c = cl'
          | otherwise = cl

    len :: Int
    len = T.length s
