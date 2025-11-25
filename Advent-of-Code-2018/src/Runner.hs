module Runner (runDay) where

import qualified Days.Day01 as Day01

runDay :: Int -> IO ()
runDay 1 = Day01.runDay01
runDay _ = putStrLn "Unknown"
