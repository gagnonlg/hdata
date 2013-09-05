{-
    Tools/Operation.hs

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

module Tools.Operation (
    Operation (..),
    isHelp,
    parseArg,
    usage
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment

data Operation = Add
               | Bookmark
               | Help
               | Modify
               | Remove
               | Search
               | View
               | Version
               deriving (Show)

isHelp :: String -> Bool
isHelp str = str `elem` ["-h","help","--help"]

parseArg :: String -> Either String Operation
parseArg op = case op of  
    "add"      -> Right Add
    "bookmark" -> Right Bookmark
    "help"     -> Right Help
    "-h"       -> Right Help
    "--help"   -> Right Help
    "modify"   -> Right Modify
    "remove"   -> Right Remove
    "search"   -> Right Search
    "view"     -> Right View
    "version"  -> Right Version
    _          -> Left $ "Invalid argument: " ++ op 

usage :: Operation -> String
usage op = show op ++ " not yet implemented" 
