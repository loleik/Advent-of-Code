module Runner (runDay) where

import Days.Day01 ( runDay01 )
import Days.Day02 ( runDay02 )
import Days.Day03 ( runDay03 )
import Days.Day04 ( runDay04 )

runDay :: Int -> IO ()
runDay 1 = runDay01
runDay 2 = runDay02
runDay 3 = runDay03
runDay 4 = runDay04
runDay _ = putStrLn "Unknown"