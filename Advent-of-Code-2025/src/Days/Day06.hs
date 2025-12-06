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

equations' :: ([Int], String) -> Int
equations' (ls,  op) =
    case head op of
        '*' -> product ls
        '+' -> sum ls
        _   -> 0

part2 :: [[Char]] -> Int -> [String] -> Int
part2 ls total (op:ops) =
    case length op - 1 of
        2 -> let (chunk, rest) = splitAt 2 ls
                 numbers       = map read chunk :: [Int]
             in part2 rest (total + equations' (numbers, op)) ops
        3 -> let (chunk, rest) = splitAt 3 ls
                 numbers       = map read chunk :: [Int]
             in part2 rest (total + equations' (numbers, op)) ops
        4 -> let (chunk, rest) = splitAt 4 ls
                 numbers       = map read chunk :: [Int]
             in part2 rest (total + equations' (numbers, op)) ops
        _ -> total
part2 _ total _ = 
    total

stripEmpty :: [[Char]] -> [[Char]]
stripEmpty ls = filter (not . null) (map (filter (/= ' ')) ls)

repl :: Char -> Char
repl ' ' = '-'
repl c   = c

splitOps :: String -> [String]
splitOps [] = []
splitOps (x:xs) =
    let (dashes, rest) = span (== '-') xs
    in (x : dashes) : splitOps rest

addChar :: Char -> [String] -> [String]
addChar c xs =
    init xs ++ [last xs ++ [c]]

runDay06 :: IO ()
runDay06 = do
    raw <- readInput "inputs/day06.txt"

    let parsed = parse raw

    let nums = stripEmpty (transpose (init raw))
    let ops = addChar '-' (splitOps (map repl (last raw)))

    let result1 = sum (map equations parsed)
    let result2 = part2 nums 0 ops
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2