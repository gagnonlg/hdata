{-
    Remove.hs

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

module Remove (
    remove,
    usageRemove
) where

import Data.Char (isDigit)

import Tools.Constants
import Tools.Operation (isHelp)

remove :: [String] -> IO ()
remove [] = error $ "remove: too few arguments ('" ++ progName ++ 
                    " remove help') for help"
remove argv | length argv > 1 = error $  "remove: too many arguments ('" ++ 
                                          progName ++ " remove help') for help"
            | isHelp (argv!!0)           = putStrLn usageRemove
            | or (map (not . isDigit) id) = error $ "remove: invalid id: " ++ id
            where id = argv!!0

usageRemove :: String
usageRemove = "usage: " ++ progName ++ " remove <id>"
