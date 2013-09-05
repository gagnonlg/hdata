{-
    Bookmark.hs    

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

module Bookmark (
    bookmark
    usageBookmark
) where

import Data.Char (isDigit)

import Tools.Constants
import Tools.Operation (isHelp)

bookmark :: [String] -> IO ()
bookmark [] = (\_ -> listAllBookmarks)
                      " bookmark help' for help)"
bookmark (x:[]) = if isHelp x 
                      then putStrLn usageBookmark
                      else if and (map isDigit x) 
                               then addToBookmarks x
                               else error $ "bookmark: invalid id: " ++ x
bookmark (x:x2:[]) = if and (map isDigit x)
                      then if x2 == "-r"
                               then removeFromBookmarks x 
                               else error $ "bookmark: invalid argument: " ++ x2
                      else error $ "bookmark: invalid id: " ++ x
bookmark _ = error $ "bookmark: too many arguments ('" ++ progName ++
                     " bookmark help' for help)"

addToBookmarks :: String -> IO ()
addToBookmarks id = return ()

listAllBookmarks :: IO ()
listAllBookmarks = return ()

removeFromBookmarks :: String -> IO ()
removeFromBookmarks is = return ()

usageBookmark :: String
usageBookmark = "usage: " ++ progName ++ " bookmark [id [options]]\n\
                \options:\n\
                \    -r : remove from bookmarks"
