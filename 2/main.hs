#!/usr/bin/env stack
{- stack script
   --resolver lts-21.22
-}

import Data.Char

main :: IO ()
main = do
  reports <- map parseLine . lines <$> getContents
  let res1 = sum $ map (fromEnum . isValid) reports

  let res2 = sum $ map (fromEnum . isAnyValid) reports

  print res1
  print res2

parseLine :: String -> [Int]
parseLine line = map read $ words line

isValid :: [Int] -> Bool
isValid xs = do
  let diffs = zipWith (-) (tail xs) xs

  let diffsValid = all ((\x -> x >= 1 && x <= 3) . abs) diffs
  let allIncreasing = all (> 0) diffs
  let allDecreasing = all (< 0) diffs

  let validReport = diffsValid && (allIncreasing || allDecreasing)
  validReport

isAnyValid :: [Int] -> Bool
isAnyValid xs = any isValid $ removeOne xs

removeOne :: [a] -> [[a]]
removeOne xs = map (`removeAt` xs) [0..length xs - 1]

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs 
