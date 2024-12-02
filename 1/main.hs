#!/usr/bin/env stack
{- stack script
   --resolver lts-21.22
-}

import Data.Char
import Data.List (sort, group)
import qualified Data.Map as Map

main :: IO ()
main = do
  inputs <- unzip . map parseLine . lines <$> getContents
  let values = uncurry zip $ sortLines inputs
  let res1 = sum $ map diff values

  let lookup = buildLookup $ snd inputs
  let res2 = sum $ map (`simscore` lookup) (fst inputs)

  print res1
  print res2

parseLine :: String -> (Int, Int)
parseLine line =
  let [a, b] = map read $ words line
   in (a, b)

sortLines :: ([Int], [Int]) -> ([Int], [Int])
sortLines (xs, ys) = (sort xs, sort ys)

diff :: (Int, Int) -> Int
diff (a, b) = abs (a - b)

buildLookup :: [Int] -> Map.Map Int Int
buildLookup = Map.fromList . map (\xs -> (head xs, length xs)) . group . sort

simscore :: Int -> Map.Map Int Int -> Int
simscore x lookup = case Map.lookup x lookup of
  Just quantity -> x * quantity
  Nothing -> 0
