module Remove (
    remove
) where

import Util

remove :: [String] -> IO ()
remove argv = putStrLn $ usage Remove
