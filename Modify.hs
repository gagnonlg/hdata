module Modify (
    modify
) where

import Util

modify :: [String] -> IO ()
modify argv = putStrLn $ usage Modify
