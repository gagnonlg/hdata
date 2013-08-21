module Search (
    search,
    usageSearch
) where

import Util

search :: [String] -> IO ()
search [] = error $ "search: no arguments specified ('" ++ progName ++ "\
                    \ search help' for help)"
search argv = if isHelp $ head argv
    then do putStrLn usageSearch
    else do putStrLn "this has not been implemented yet"

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
