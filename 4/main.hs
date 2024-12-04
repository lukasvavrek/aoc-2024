#!/usr/bin/env stack
{- stack script
   --resolver lts-21.22
-}

import Data.Char

main :: IO ()
main = do
    input <- lines <$> getContents
    let xs = findChar input 'X'
    let xmasCount = sum $ map (containsXmas input) xs
    print xmasCount

    let as = findChar input 'A'
    let xmasCount' = length $ filter (isXmas' input) as
    print xmasCount'


data Coord = 
    X Int Int
    deriving (Show)

findChar :: [String] -> Char -> [Coord]
findChar input ch =
    [X x y | (y, row) <- zip [0..] input, x <- findX row]
    where
        findX row = [x | (x, c) <- zip [0..] row, c == ch]


containsXmas :: [String] -> Coord -> Int
containsXmas input (X x y) =
    length $ filter (isXmas input (X x y)) 
        [(1, 0), (-1, 0), 
         (0, 1), (0, -1), 
         (1, 1), (1, -1), 
         (-1, 1), (-1, -1)]

isXmas :: [String] -> Coord -> (Int, Int) -> Bool
isXmas input (X x y) (xa, ya) =
    getAt input (x, y) == Just 'X' &&
    getAt input (x + xa, y + ya) == Just 'M' &&
    getAt input (x + 2 * xa, y + 2 * ya) == Just 'A' &&
    getAt input (x + 3 * xa, y + 3 * ya) == Just 'S'

isXmas' :: [String] -> Coord -> Bool
isXmas' input (X x y) =
    ((getAt input (x - 1, y - 1) == Just 'S' &&
      getAt input (x + 1, y + 1) == Just 'M') ||

     (getAt input (x - 1, y - 1) == Just 'M' &&
      getAt input (x + 1, y + 1) == Just 'S')) &&

    ((getAt input (x - 1, y + 1) == Just 'S' &&
      getAt input (x + 1, y - 1) == Just 'M') ||

     (getAt input (x - 1, y + 1) == Just 'M' &&
      getAt input (x + 1, y - 1) == Just 'S'))

getAt :: [String] -> (Int, Int) -> Maybe Char
getAt input (x, y)
    | y < 0 || y >= length input  = Nothing
    | x < 0 || x >= length (input !! y) = Nothing
    | otherwise = Just $ (input !! y) !! x
