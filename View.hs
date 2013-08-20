module View (
    view
) where

import Util

view :: [String] -> IO ()
view argv = putStrLn $ usage View
