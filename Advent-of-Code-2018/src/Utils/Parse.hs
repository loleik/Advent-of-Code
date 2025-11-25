module Utils.Parse (readInput) where


readInput :: String -> IO [String]
readInput x = lines <$> readFile x