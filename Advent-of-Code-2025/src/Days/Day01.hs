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

runTurns :: [String] -> [(Int, Int, Int)]
runTurns l =
    scanl
        (\(cur, _, _) delta -> (modularAdd cur delta, cur, delta))
        (50, 50, 0)
        (map splitTurns l)

part1 :: [(Int, Int, Int)] -> Int
part1 l = length [ total | (total, _, _) <- l, total == 0 ]

part2Cond :: (Int, Int, Int) -> Bool
part2Cond (_, before, delta) =
    let raw = before + delta
    in raw < 0 || raw > 99 && mod raw 100 /= 0

part2 :: [(Int, Int, Int)] -> Int
part2 = length . filter part2Cond

runDay01 :: IO ()
runDay01 = do
    input <- readInput "inputs/day01.txt"
    let turns = runTurns input

    let result1 = part1 turns
    putStrLn $ "Part 1: " ++ show result1

    let result2 = result1 + part2 turns
    putStrLn $ "Part 2: " ++ show result2