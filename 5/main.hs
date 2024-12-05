#!/usr/bin/env stack
{- stack script
   --resolver lts-21.22
   --package containers
   --package split
-}

import Control.Monad (foldM)
import Data.Char
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace

main :: IO ()
main = do
  input <- lines <$> getContents
  let instructions = map parseRule $ takeWhile (/= "") input
  let rules = buildRulesMap instructions
  let updates = map parseUpdate $ drop 1 $ dropWhile (/= "") input

  let res1 = sum $ map getMiddle $ filter (isValidUpdate rules) updates
  print res1
  where
    getMiddle xs = xs !! (length xs `div` 2)

data Rule
  = Rule Int Int
  deriving (Show)

parseRule :: String -> Rule
parseRule str =
  let [a, b] = map (\x -> read x :: Int) $ splitOn "|" str
   in Rule a b

buildRulesMap :: [Rule] -> Map.Map Int [Rule]
buildRulesMap = foldr addRule Map.empty
  where
    addRule r@(Rule x y) map =
      Map.insertWith (++) y [r] $
        Map.insertWith (++) x [r] map

parseUpdate :: String -> [Int]
parseUpdate str =
  map (\x -> read x :: Int) $ splitOn "," str

isValidUpdate :: Map.Map Int [Rule] -> [Int] -> Bool
isValidUpdate rmap xs =
  case foldM checkOne Set.empty xs of
    Just _ -> True
    Nothing -> False
  where
    checkOne :: Set.Set Int -> Int -> Maybe (Set.Set Int)
    checkOne visited x = do
      let visited' = Set.insert x visited
      let rules = fromMaybe [] $ Map.lookup x rmap
      let invalid = any (\rule -> not $ isRuleOk visited' rule x) rules
      if invalid then Nothing else Just visited'

isRuleOk :: Set.Set Int -> Rule -> Int -> Bool
isRuleOk visited (Rule x y) num =
  if x == num
    then not $ Set.member y visited
    else True
