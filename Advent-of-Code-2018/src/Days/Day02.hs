module Days.Day02 (runDay02) where

import Utils.Parse ( readInput )
import qualified Data.Map as M
import Data.Maybe ( listToMaybe )

createMap :: String -> M.Map Char Int
createMap = foldl update M.empty
    where
        update m c = M.insertWith (+) c 1 m

findMatches :: M.Map Char Int -> (Bool, Bool)
findMatches m = (has2, has3)
    where
        vals = M.elems m
        has2 = 2 `elem` vals
        has3 = 3 `elem` vals

countPairs :: [(Bool, Bool)] -> (Int, Int)
countPairs = foldl add (0, 0)
    where
        add (a, b) (x, y) =
            (a + if x then 1 else 0,
             b + if y then 1 else 0)

part1 :: [String] -> Int
part1 xs =
    let pairs = map (findMatches . createMap) xs
        totals = countPairs pairs
    in uncurry (*) totals

differences :: String -> String -> Int
differences xs ys = length $ filter id $ zipWith (/=) xs ys

commonChars :: String -> String -> String
commonChars xs xy = map fst $ filter (uncurry (==)) $ zip xs xy

findPair :: [String] -> Maybe (String, String)
findPair ids = 
    listToMaybe
        [ (x, y)
        | x <- ids
        , y <- ids
        , x /= y
        , differences x y == 1
        ]

part2 :: [String] -> String
part2 ids =
    case findPair ids of
        Just (a, b) -> commonChars a b
        Nothing     -> ""

runDay02 :: IO ()
runDay02 = do
    input <- readInput "inputs/day02.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)