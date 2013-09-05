{-
    View.hs

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

module View (
    view
) where

import Data.Char (isDigit)

import Tools.Constants
import Tools.Operation (isHelp)

view :: [String] -> IO ()

view [] = error errTooFew

view (x:[]) | isHelp x            = putStrLn usageView
            | and (map isDigit x) = doView x ""
            | otherwise           = error $ "view: invalid id: " ++ x

view (x:xs) = do
    if and (map isDigit x)
        then case tryGetViewer xs of
             Left msg -> error $ "view: " ++ msg
             Right v  -> doView x v 
        else error $ "view: invalid id: " ++ x


doView :: String -> String -> IO ()
doView id viewer = putStrLn "not yet implemented"

tryGetViewer :: [String] -> Either String String
tryGetViewer (f:[])   | f == "-v" = Left $ "too few arguments ('" ++ progName ++ 
                                           " view help' for help)"
                      | otherwise = Left $ "invalid argument: " ++ f
tryGetViewer (f:v:[]) | f == "-v" = Right v
                      | otherwise = Left $ "invalid argument: " ++ f
tryGetViewer _ = Left $ "too many arguments ('" ++ progName ++ " view help' for help)"


errTooFew :: String
errTooFew = "view: too few arguments ('" ++ progName ++ " view help' for help)"

usageView :: String
usageView = "usage: " ++ progName ++ " view <id> [options]\n\
            \options:\n\
            \    -v <viewer>"
