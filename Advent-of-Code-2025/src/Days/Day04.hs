module Days.Day04 ( runDay04 ) where 

import Utils.Parse ( readInput )

import Data.Vector ((!?))
import qualified Data.Vector as V

type Matrix a = V.Vector (V.Vector a)

offsets :: [(Int, Int)]
offsets = 
    [
        (-1, -1), (-1, 0), (-1, 1),
        (0, -1),           (0, 1),
        (1, -1),  (1, 0),  (1, 1)
    ]

checkIndex :: Matrix Char -> (Int, Int) -> Maybe Char
checkIndex m (r, c) = do
    row <- m !? r
    row !? c

neighbours :: Matrix Char -> (Int, Int) -> [Char]
neighbours m (r, c) =
    filter (=='@') [ val
        | (dr, dc) <- offsets,
          Just val <- [ checkIndex m (r + dr, c + dc) ]
    ]

isAtSign :: Matrix Char -> (Int, Int) -> [Char]
isAtSign m (r, c) =
    case checkIndex m (r, c) of
        Just '@' -> neighbours m (r, c)
        _        -> "@@@@@@@@"

allNeighbours :: Matrix Char -> V.Vector (V.Vector [Char])
allNeighbours m = 
    V.imap (\r row ->
        V.imap (\c _ -> isAtSign m  (r,c)) row
    ) m

part1 :: Matrix Char -> Int
part1 m = 
    let ns       = V.concatMap id (allNeighbours m)
        filtered = V.filter (\xs -> length xs < 4) ns
    in length filtered

runDay04 :: IO ()
runDay04 = do
    raw <- readInput "inputs/day04.txt"

    let matrix = V.fromList (map V.fromList raw)

    let result1 = part1 matrix

    putStrLn $ "Part 1: " ++ show result1

    putStrLn "Part 2: "