module Bookmark (
    bookmark
) where

import Util

bookmark :: [String] -> IO ()
bookmark argv = putStrLn $ usage Bookmark
