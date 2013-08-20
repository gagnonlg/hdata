module Add (
    add,
    usageAdd
) where

import Util

add :: [String] -> IO ()
add argv = putStrLn $ usageAdd

usageAdd :: String
usageAdd = "usage: " ++ progName ++ " add <filters>\n\
            \filters:\n\
            \    -f <file>\n\
            \    -t <title>\n\
            \    -a <author1 [author2] ...>\n\
            \    -k <keyword1 [keyword2] ...>\n\
            \    -j <journal>\n\
            \    -i <issue>\n\
            \    -d <date>\n\
            \    -p <page-from>-<page-to>"
