module Days.Day02 (runDay02, matches, expandRange) where

import Utils.Parse ( readInput, splitting )
import qualified Data.Text as T
import Text.Regex.PCRE ((=~))

expandRange :: [T.Text] -> [String]
expandRange s =
    let lo = read (T.unpack (head s)) :: Int
        hi = read (T.unpack (last s)) :: Int
    in
        map show [lo .. hi]

matches :: String -> [String] -> [String]
matches pattern = filter (=~ pattern)

sumIDs :: [String] -> Int
sumIDs = sum . map read

runDay02 :: IO ()
runDay02 = do
    raw <- readInput "inputs/day02.txt"
    let parsed = concatMap (splitting ',' . T.pack) raw
    let split = map (splitting '-') parsed
    let expanded = concatMap expandRange split

    let invalidIDs1 = matches "^(\\d+)\\1$" expanded
    let result1 = sumIDs invalidIDs1

    let invalidIDs2 = matches "^(\\d+)\\1+$" expanded
    let result2 = sumIDs invalidIDs2

    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2