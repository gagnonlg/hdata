module Search (
    search
) where

import Util

search :: [String] -> IO ()
search argv = putStrLn $ usage Search
