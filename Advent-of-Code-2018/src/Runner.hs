module Runner (runDay) where

import qualified Days.Day01 as Day01
import qualified Days.Day02 as Day02

runDay :: Int -> IO ()
runDay 1 = Day01.runDay01
runDay 2 = Day02.runDay02
runDay _ = putStrLn "Unknown"
