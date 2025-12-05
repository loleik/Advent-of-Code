module Days.Day05 ( runDay05 ) where 

import Utils.Parse ( readInput, splitting )
import Days.Day02 ( matches )
import qualified Data.Text as T
import Data.Set (fromList, toList)
import Data.List (sortBy)
import Data.Ord (comparing)

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

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges (r:rs) = go r rs
    where
        go current [] = [current]
        go (lo1, hi1) ((lo2, hi2):rest)
            | lo2 <= hi1 + 1 = go (lo1, max hi1 hi2) rest
            | otherwise      = (lo1, hi1) : go (lo2, hi2) rest

internalSize :: (Int, Int) -> Int
internalSize (lo, hi) = hi - lo + 1

part2 :: [String] -> Int
part2 ranges = 
    let xs    = concatMap (parseRange . splitting '-' . T.pack) ranges
        ms    = mergeRanges (sortBy (comparing fst) xs)
        sizes = map internalSize ms
    in sum sizes

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