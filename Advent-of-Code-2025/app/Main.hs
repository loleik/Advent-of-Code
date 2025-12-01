module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Runner (runDay)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:_) ->
            case readMaybe x :: Maybe Int of
                Just day | day >= 1 && day <= 25 -> runDay day
                Just _  -> putStrLn "Error: first argument (day) should be between 1 and 25"
                Nothing -> putStrLn "Error: first argument (day) should be a number"
        [] ->
            putStrLn "Error: no arguments provided"