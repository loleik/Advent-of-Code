module Days.Day05 ( runDay05 ) where 

import Utils.Parse ( readInput, splitting )
import Days.Day02 ( matches, expandRange )
import qualified Data.Text as T
import Data.Set (fromList, toList)

parseRange :: [T.Text] -> [(Int, Int)]
parseRange s =
    let lo = read (T.unpack (head s)) :: Int
        hi = read (T.unpack (last s)) :: Int
    in [(lo, hi)]

freshIDs :: [String] -> [(Int, Int)]
freshIDs s =
    let inter = map (splitting '-' . T.pack) s
    in dupes (concatMap parseRange inter)

dupes :: (Ord a) => [a] -> [a]
dupes = toList . fromList

checkRange :: Int -> [(Int, Int)] -> Bool
checkRange n = any (\(lo, hi) -> n >= lo && n <= hi)

part1 :: [Int] -> [(Int, Int)] -> Int
part1 ids ranges =
    length (filter (`checkRange` ranges) ids)

part2 :: [String] -> Int
part2 ranges = 
    let xs    = map (splitting '-' . T.pack) ranges
        final = concatMap expandRange xs
    in length (dupes final)

runDay05 :: IO ()
runDay05 = do
    raw <- readInput "inputs/day05.txt"

    let ranges = matches "^(\\d+-\\d+)$" raw
    let ids = map read (matches "^(\\d+)$" raw)
    let expanded = freshIDs ranges

    let result1  = part1 ids expanded
    putStrLn $ "Part 1: " ++ show result1

    let result2 = part2 ranges
    putStrLn $ "Part 2: " ++ show result2