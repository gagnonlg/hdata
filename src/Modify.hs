{-
    Modify.hs

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

module Modify (
    modify
) where

import Data.Char (isDigit)

import Tools.Constants
import Tools.Filter (tryGetFilters)
import Tools.Operation (isHelp)
import Tools.SQL (getEntry)

modify :: [String] -> IO ()
modify []      = error errTooFew
modify (f:[])  = if isHelp f then putStrLn usageModify else error errTooFew
modify (id:fs) = if or (map (not . isDigit) id) 
                     then error $ "modify: invalid id: " ++ id
                     else do entry <- getEntry $ read id
                             case entry of
                                 Left msg -> error $ "modify: " ++ msg
                                 Right row -> do filters <- tryGetFilters fs 
                                                 case filters of
                                                    Left msg -> error $ "modify: " ++ msg
                                                    --Right fs' -> doModify row fs'
                                                    Right fs' -> putStrLn "not yet implemented"


errTooFew :: String 
errTooFew = "modify: too few arguments ('" ++ progName ++ " modify help' for help)"

usageModify :: String
usageModify = "usage: " ++ progName ++ " modify <id> <filters>\n\
              \filters:\n\
              \    -f <new file>\n\
              \    -t <new title>\n\
              \    -a +<author-add> <author-remove>\n\
              \    -k +<keyword-add> <keyword-remove>\n\
              \    -j <new journal>\n\
              \    -v <new volume>\n\
              \    -y <new year>\n\
              \    -p <new page-from> <new page-to>\n\
              \    -b"
