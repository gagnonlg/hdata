{-
    Tools/Filter.hs

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

module Tools.Filter (
) where

import Data.Char (isDigit)
import Data.List (intersperse)

data Filter = File       String
            | Title      String
            | Authors    String
            | Keywords   String
            | Journal    String
            | Volume     String
            | Year       String
            | Pages      String
            | Bookmarked String
            deriving (Show)

isFilter :: String -> Bool
isFilter f = f `elem` ["-f","-t","-a","-k","-j","-v","-y","-p","-b"]

isLikeFilter :: String -> Bool
isLikeFilter f = (length f == 2) && (head f == '-') 

isYear :: String -> Bool
isYear s = (length s == 4) && (and $ map isDigit s)
                   
getFilters :: [String] -> Either String [Filter]
getFilters strs = case getFilterPairs strs of
    Left  msg   -> Left msg
    Right pairs -> mapToFilter pairs 

getFilterPairs :: [String] -> Either String [(String,[String])]
getFilterPairs strs = worker [] strs
    where worker fs [] = Right fs
          worker fs (x:xs) | not (isFilter x)  = Left $ "Invalid argument: " ++ x 
                           | otherwise         = worker ((x, values):fs) rest
                           where (values,rest) = break isLikeFilter xs

toFilter :: (String,[String]) -> Either String Filter
toFilter (f,vs) | null vs = if f == "-b"
                                then Right $ Bookmarked "true"
                                else Left "too few arguments"
                | otherwise = case f of
    "-f" -> Right $ File     $ concat $ intersperse " " vs
    "-t" -> Right $ Title    $ concat $ intersperse " " vs
    "-a" -> Right $ Authors  $ concat $ intersperse " | " vs
    "-k" -> Right $ Keywords $ concat $ intersperse " | " vs
    "-j" -> Right $ Journal  $ concat $ intersperse " " vs
    "-v" -> if length vs == 1
                then if and $ map isDigit vs0
                        then Right $ Volume vs0
                        else Left  $ "Invalid Volume: " ++ vs0
                else Left "too many arguments to -v"
    "-y" -> if length vs == 1
                then if isYear vs0
                        then Right $ Year vs0
                        else Left  $ "Invalid year: " ++ vs0
                else Left "too many arguments to -y"
    "-p" -> case length vs of
                1 -> if and $ map isDigit vs0
                        then Right $ Pages vs0
                        else Left  $ "Invalid pages: " ++ vs0
                2 -> if and $ map isDigit (vs0 ++ vs1)
                        then Right $ Pages (vs0 ++ " " ++ vs1)
                        else Left  $ "Invalid pages: " ++ (vs0 ++ " " ++ vs1)
                _ -> Left "too many arguments to -p"     
    "-b" -> if null vs 
                then Right $ Bookmarked "true"
                else Left "too many arguments to -b"
    where vs0 = vs!!0
          vs1 = vs!!1

mapToFilter :: [(String,[String])] -> Either String [Filter]
mapToFilter strs = worker [] strs
    where worker fs []     = Right fs
          worker fs (x:xs) = let f = toFilter x 
                             in case f of
                                    Left msg -> Left msg
                                    Right f  -> worker (f:fs) xs 

usageFilters :: String
usageFilters = "filters:\n\
               \    -f <file>\n\
               \    -t <title>\n\
               \    -a <authors>\n\
               \    -k <keywords>\n\
               \    -j <journal>\n\
               \    -y <year> : <yyyy>\n\
               \    -p <page> [page]"
