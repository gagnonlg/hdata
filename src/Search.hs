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

import Data.Char (isDigit)
import Data.List (intersperse)

import Tools.Constants
import Tools.Filter (rowToString,usageFilters)
import Tools.Operation (isHelp)
import Tools.SQL (getAllEntries,getEntry)

search :: [String] -> IO ()

search [] = do
    entries <- getAllEntries
    putStr $ concat $ intersperse "\n" $ map rowToString entries

search (x:[]) | and $ map isDigit x = do 
                    entry <- getEntry $ read x
                    case entry of 
                        Left msg  -> error $ "search: " ++ msg
                        Right row -> putStr $ rowToString row
              | isHelp x  = putStrLn usageSearch
              | otherwise = error $ "search: invalid id: " ++ x

usageSearch :: String
usageSearch = "usage: " ++ progName ++ " search <filters>\n\
              \or:\n\
              \       " ++ progName ++ " search [id]\n" ++ usageFilters
