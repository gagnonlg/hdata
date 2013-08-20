module Add (
    add
) where

import Util

add :: [String] -> IO ()
add argv = putStrLn $ usage Add

