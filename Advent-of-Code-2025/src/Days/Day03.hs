module Days.Day03 ( runDay03 ) where 

import Utils.Parse ( readInput )

maxJolts :: String -> String
maxJolts s = 
    case snd (foldr digitScan (Nothing, Nothing) s) of
        Just (a, b) -> [a, b]
        Nothing     -> ['0', '0']

digitScan :: Char 
             -> (Maybe Char, Maybe (Char, Char)) 
             -> (Maybe Char, Maybe (Char, Char))
digitScan d (Nothing, bestPair) = (Just d, bestPair)
digitScan d (Just bestSuffix, bestPair) =
    let candidate   = (d, bestSuffix)
        bestPair'   = case bestPair of
            Nothing      -> Just candidate
            Just current -> Just (max current candidate)
        bestSuffix' = Just (max d bestSuffix)
    in
        (bestSuffix', bestPair')

part1 :: [String] -> Int
part1 = sum . map (read . maxJolts)

runDay03 :: IO ()
runDay03 = do
    raw <- readInput "inputs/day03.txt"

    let result1 = part1 raw
    putStrLn $ "Part 1: " ++ show result1
    putStrLn "Part 2: "