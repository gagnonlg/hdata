{-
    hdata.hs

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

module Main where

import System.Environment

import Add
import Bookmark
import Citation
import Modify
import Remove
import Search
import Tools.Constants
import Tools.Operation
import View

help :: [String] -> IO ()
help argv = putStrLn $ usageHelp

usageHelp = "usage: " ++ progName ++ " [operation] [id]\n\
             \operations:\n\
             \    add                  <filters>     \n\
             \    bookmark [options]             [id]\n\    
             \    citation [options]             [id]\n\    
             \    help     [operation]               \n\    
             \    modify   [options]   <filters> <id>\n\    
             \    remove   [options]             <id>\n\    
             \    search   [options]   [filters] [id]\n\    
             \    view     [options]             <id>\n\    
             \    version"   

version :: IO ()
version = do 
    putStrLn $ progName ++ " v" ++ progVersion
    putStrLn "© 2013 Louis-Guillaume Gagnon - GPLv3+"
             

main :: IO ()
main = do 
    argv <- getArgs
    if null argv 
    then 
      do 
        error $ "No operation specified ('" ++ progName ++ " help' for help)"
    else
      do
        let argv' = tail argv 
        case parseArg (head argv) of
            Right Add      -> add argv'
            Right Bookmark -> bookmark argv'
            Right Citation -> citation argv'
            Right Help     -> help argv'
            Right Modify   -> modify argv'
            Right Remove   -> remove argv'
            Right Search   -> search argv'
            Right View     -> view argv'
            Right Version  -> version
            Left msg       -> error msg

    


