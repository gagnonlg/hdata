module Citation (
    citation
) where

import Util

citation :: [String] -> IO ()
citation argv = putStrLn $ usage Citation
