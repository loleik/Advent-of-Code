module Days.Day07 ( runDay07 ) where 

import Utils.Parse ( readInput )


runDay07 :: IO ()
runDay07 = do
    raw <- readInput "inputs/day07.txt"

    mapM_ print raw

    putStrLn $ "Part 1: "
    putStrLn $ "Part 2: "