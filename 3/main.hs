#!/usr/bin/env stack
{- stack script
   --resolver lts-21.22
   --package regex-tdfa
   --package mtl
-}

import Data.Char
import Text.Regex.TDFA ((=~), getAllTextMatches)
import Data.List (isPrefixOf)
import Control.Monad.State

main :: IO ()
main = do
    memory <- getContents
    print $ sum $ map (uncurry (*)) $ findMuls memory

    let commands = parseCommands memory
    let ((), state) = runState (interpreter commands) initialState
    print $ accumulator state

findMuls :: String -> [(Int, Int)]
findMuls str = 
    [(read x :: Int, read y :: Int) | [_, x, y] <- str =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]]

data Command = 
    Mul Int Int
    | Dont
    | Do
    deriving (Show)

parseCommands :: String -> [Command]
parseCommands str = 
    map parseCommand matches
    where
        pattern = "(mul\\([0-9]+,[0-9]+\\)|don't\\(\\)|do\\(\\))"
        matches = getAllTextMatches (str =~ pattern) :: [String]
        parseCommand s
            | "mul(" `isPrefixOf` s = 
                let nums = getAllTextMatches (s =~ "[0-9]+") :: [String]
                in Mul (read (nums !! 0) :: Int) (read (nums !! 1) :: Int)
            | "don't()" == s = Dont
            | "do()" == s = Do
            | otherwise = error $ "Unknown command: " ++ s

data MachineState = MachineState
    { isEnabled :: Bool
    , accumulator :: Int
    } deriving (Show)

initialState :: MachineState
initialState = MachineState
    { isEnabled = True
    , accumulator = 0
    }

executeCommand :: Command -> State MachineState ()
executeCommand (Mul x y) = do
    state <- get
    let acc = accumulator state
    if isEnabled state
        then put $ state { accumulator = acc + x * y }
        else return ()

executeCommand Dont = do
    state <- get
    put $ state { isEnabled = False }

executeCommand Do = do
    state <- get
    put $ state { isEnabled = True }

interpreter :: [Command] -> State MachineState ()
interpreter = mapM_ executeCommand
