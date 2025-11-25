module Days.Day01 (runDay01) where

import Utils.Parse ( readInput )
import qualified Data.Set as S

part1 :: String -> Int
part1 ('-':xs) = read ('-':xs)
part1 ('+':xs) = read xs
part1 xs = read xs

part2 :: [Int] -> Int
part2 xs = go S.empty (scanl (+) 0 (cycle xs)) where
    go seen (y:ys)
        | y `S.member` seen = y
        |         otherwise = go (S.insert y seen) ys
    go _ [] = error "Impossible - scanl with cycle is infinite"

runDay01 :: IO ()
runDay01 = do
    input <- readInput "inputs/day01.txt"
    let values = map part1 input
    putStrLn $ "Part 1: " ++  show (sum values)

    let repeated = part2 values
    putStrLn $ "Part 2: " ++ show repeated