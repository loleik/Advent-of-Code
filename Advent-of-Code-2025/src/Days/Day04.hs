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

allNeighbours :: Matrix Char -> V.Vector (V.Vector ((Int, Int), [Char]))
allNeighbours m = 
    V.imap (\r row ->
        V.imap (\c _ -> ((r, c), isAtSign m  (r,c))) row
    ) m

findCells :: Matrix Char -> V.Vector ((Int, Int), [Char])
findCells m = 
    let flattened = V.concatMap id (allNeighbours m)
    in V.filter (\(_, xs) -> length xs < 4) flattened

removeRolls :: Matrix Char -> V.Vector (Int, Int) -> Matrix Char
removeRolls m coords = 
    V.imap (\r row ->
        V.imap (\c val ->
            if (r, c) `elem` coords
                then '.'
                else val
        ) row
    ) m

countDiff :: Eq a => Matrix a -> Matrix a -> Int
countDiff m1 m2 =
    V.sum $
        V.zipWith
            (\row1 row2 ->
                V.length $ V.filter id $
                    V.zipWith (/=) row1 row2    
            )
            m1
            m2

part1 :: Matrix Char -> Int
part1 m = V.length (findCells m)

part2 :: Matrix Char -> Matrix Char
part2 m = 
    let coords = V.map fst (findCells m)
    in if not (null coords)
        then part2 (removeRolls m coords)
        else removeRolls m coords

runDay04 :: IO ()
runDay04 = do
    raw <- readInput "inputs/day04.txt"
    let matrix = V.fromList (map V.fromList raw)

    let result1 = part1 matrix
    putStrLn $ "Part 1: " ++ show result1

    let result2 = countDiff matrix (part2 matrix)
    putStrLn $ "Part 2: " ++ show result2