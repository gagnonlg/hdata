{-
    Add.hs    

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

module Add (
    add,
    usageAdd
) where

import Data.Char
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory

import Tools.Constants
import Tools.Filter (tryGetFilters, usageFilters)
import Tools.Operation (isHelp)
import Tools.SQL (addEntry)

add :: [String] -> IO ()
add [] = error $ "add: no arguments specified ('" ++ progName ++ " add help' for help)"
add argv = if isHelp $ head argv 
    then do putStrLn usageAdd 
    else do filters <- tryGetFilters argv
            case filters of
                Left  msg -> error $ "add: " ++ msg
                Right fs  -> addEntry fs

usageAdd :: String
usageAdd = "usage: " ++ progName ++ " add <filters>\n" ++ usageFilters
