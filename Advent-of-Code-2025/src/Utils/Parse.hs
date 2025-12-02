module Utils.Parse (readInput, splitting) where

import qualified Data.Text as T

splitting :: Char -> T.Text -> [T.Text]
splitting c = T.split (==c)

readInput :: String -> IO [String]
readInput x = lines <$> readFile x