#!/usr/bin/env stack
{- stack script
   --resolver lts-21.22
   --package regex-tdfa
-}

import Data.Char
import Text.Regex.TDFA ((=~))

findMuls :: String -> [(Int, Int)]
findMuls str = 
    [(read x :: Int, read y :: Int) | [_, x, y] <- str =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]]

main :: IO ()
main = do
    memory <- getContents
    print $ sum $ map (uncurry (*)) $ findMuls memory
