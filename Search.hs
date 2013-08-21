module Search (
    search,
    usageSearch
) where

import Util

search :: [String] -> IO ()
search argv = putStrLn $ usageSearch

usageSearch :: String
usageSearch = "usage: " ++ progName ++ " search <filters>\n\
              \or:\n\
              \       " ++ progName ++ " search [id]\n\
              \filters:\n\
              \    -f <file>\n\
              \    -t <title>\n\
              \    -a <author1 [author2] ...>\n\
              \    -k <keyword1 [keyword2] ...>\n\
              \    -j <journal>\n\
              \    -v <volume>\n\
              \    -i <issue>\n\
              \    -d <date-from> <date-to>\n\
              \    -p <page-from> <page-to>\n\
              \    -b : bookmarks"
