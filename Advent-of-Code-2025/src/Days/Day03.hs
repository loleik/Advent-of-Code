module Days.Day03 ( runDay03 ) where 

import Utils.Parse ( readInput )

type Acc = (String, Int)

popLessThan :: Acc -> Char -> Acc
popLessThan ("", k) _    = ("", k)
popLessThan (x:xs, k) d
    | k > 0 && x < d = popLessThan (xs, k - 1) d
    | otherwise      = (x:xs, k)

step :: Acc -> Char -> Acc
step (stack, k) d = 
    let (stack', k') = popLessThan (stack, k) d
    in (d : stack', k')

monotonicStack :: String -> Int -> String
monotonicStack s i = 
    let removals   = length s - i
        (stack, k) = foldl' step ("", removals) s
        trimmed    = drop k stack
    in reverse trimmed

wrapper :: [String] -> Int -> Int
wrapper l x = sum (map (read . (`monotonicStack` x)) l)

runDay03 :: IO ()
runDay03 = do
    raw <- readInput "inputs/day03.txt"

    let result1 = wrapper raw 2
    putStrLn $ "Part 1: " ++ show result1

    let result2 = wrapper raw 12
    putStrLn $ "Part 2: " ++ show result2