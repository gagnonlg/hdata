{-
    Search.hs

    Copyright 2013 Louis-Guillaume Gagnon <louis.guillaume.gagnon@gmail.com>    

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Search (
    search,
    usageSearch
) where

import Tools.Constants
import Tools.Filter (usageFilters)
import Tools.Operation (isHelp)

search :: [String] -> IO ()
search [] = error $ "search: no arguments specified ('" ++ progName ++ "\
                    \ search help' for help)"
search argv = if isHelp $ head argv
    then do putStrLn usageSearch
    else do putStrLn "this has not been implemented yet"

usageSearch :: String
usageSearch = "usage: " ++ progName ++ " search <filters>\n\
              \or:\n\
              \       " ++ progName ++ " search [id]\n" ++ usageFilters
