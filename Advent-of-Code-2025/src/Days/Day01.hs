module Days.Day01 (runDay01) where

import Utils.Parse ( readInput )

splitTurns :: String -> Int
splitTurns (d:n)
    | d == 'R'  = read n
    | d == 'L'  = - read n
    | otherwise = 0
splitTurns _ = 0

modularAdd :: Int -> Int -> Int
modularAdd x y = mod (x + y) 100

part1 :: [String] -> Int
part1 =
    length
    . filter (== 0)
    . scanl modularAdd 50
    . map splitTurns

runDay01 :: IO ()
runDay01 = do
    input <- readInput "inputs/day01.txt"
    let output = part1 input
    putStrLn $ "Part 1: " ++ show output

    putStrLn "Part 2: "