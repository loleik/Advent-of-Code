module Days.Day06 ( runDay06 ) where 

import Utils.Parse ( readInput, splitting )
import qualified Data.Text as T
import Data.List (transpose)

parse :: [String] -> [[T.Text]]
parse ls =
    let split    = map (splitting ' ' . T.pack) ls
        filtered = map (filter (not . T.null)) split

    in transpose filtered

add :: [T.Text] -> Int
add args = 
    let nums = map (read . T.unpack) args :: [Int]
    in sum nums

mult :: [T.Text] -> Int
mult args =
    let nums = map (read . T.unpack) args :: [Int]
    in product nums

equations :: [T.Text] -> Int
equations ls =
    case last ls of 
        t | t == T.singleton '*' -> mult (init ls)
          | t == T.singleton '+' -> add (init ls)
          | otherwise            -> 0

runDay06 :: IO ()
runDay06 = do
    raw <- readInput "inputs/day06.txt"

    let parsed = parse raw

    let result1 = sum (map equations parsed)
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " 