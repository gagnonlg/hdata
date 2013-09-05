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
    filtA,
    filtK,
    filtP,
    getPath,
    getValues,
    rowToString,
    separateMulti,
    tryGetFilters,
    updatePairs,
    usageFilters
) where

import Data.Char (isDigit)
import Data.List (intersect,intersperse,(\\))
import System.Directory (doesFileExist)

data Filter = File       String
            | Title      String
            | Authors    String
            | Keywords   String
            | Journal    String
            | Volume     String
            | Year       String
            | Pages      String
            deriving (Show)

tryGetFilters :: [String] -> IO (Either String ([String],[String]));
tryGetFilters argv = case getFilters argv of
    Left msg    -> return $ Left msg
    Right pairs -> if anyDuplicates pairs
                       then return $ Left "duplicated arguments"
                       else  do
        s <- checkFile pairs
        case s of 
            Right _  -> return $ Right $ pairFilters pairs
            Left msg -> return $ Left msg 

areFiltersEqual :: Filter -> Filter -> Bool
areFiltersEqual f1 f2 = f1' == f2' 
    where (f1',_) = break (==' ') $ show f1
          (f2',_) = break (==' ') $ show f2

anyDuplicates :: [Filter] -> Bool
anyDuplicates (f:[]) = False
anyDuplicates (f:fs) = if or $ map (areFiltersEqual f) fs
                             then True
                             else do anyDuplicates fs

checkFile :: [Filter] -> IO (Either String ()) 
checkFile fs = case filter isPathFilter fs of
    []     -> return $ Right ()
    ((File p):_)  -> do exists <- doesFileExist p
                        if exists 
                            then do return $ Right ()
                            else do return $ Left $ "File does not exists: " ++ p   

isFilter :: String -> Bool
isFilter f = f `elem` ["-f","-t","-a","-k","-j","-v","-y","-p"]

isLikeFilter :: String -> Bool
isLikeFilter f = (length f == 2) && (head f == '-') 

isPathFilter :: Filter -> Bool
isPathFilter f = case f of
    File _ -> True
    _      -> False

isYear :: String -> Bool
isYear s = (length s == 4) && (and $ map isDigit s)

filtA :: String -> [[String]] -> [[String]]
filtA [] rs = rs
filtA xs rs = map filtA' rs
    where filtA' row = let xas = multiToList xs 
                           ras = multiToList (row!!3)    
                       in if null $ intersect xas ras
                             then [];
                             else row
 
filtK :: String -> [[String]] -> [[String]]
filtK [] rs = rs
filtK xs rs = map filtK' rs
    where filtK' row = let xas = multiToList xs 
                           ras = multiToList (row!!4)    
                       in if null $ intersect xas ras
                             then [];
                             else row

filtP :: String -> [[String]] -> [[String]]
filtP [] rs = rs
filtP xs rs = map filtP' rs
    where filtP' row = let xas = multiToList xs 
                           ras = multiToList (row!!8)    
                       in if null $ intersect xas ras
                             then [];
                             else row

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

getPath :: [String] -> IO (Either String String)
getPath row = do
    case row !! 1 of 
        "" -> return $ Left "no path for this entry"
        f  -> doesFileExist f >>= \p -> if p 
                                       then return $ Right f
                                       else return $ Left $ "no file at path: " ++ f

getValues :: String -> ([String],[String]) -> String
getValues _ ([],[]) = []
getValues key ((k:ks),(v:vs)) | k == key  = v
                              | otherwise = getValues key (ks,vs)

pairFilters :: [Filter] -> ([String],[String])
pairFilters fs = worker [] [] fs 
    where worker ks vs []     = (ks,vs)
          worker ks vs (f:fs) = worker (k:ks) (v:vs) fs
              where (k,v') = break (==' ') $ show f
                    v      = filter (/='\"') (tail v')

rowToString :: [String] -> String
rowToString row = concat $ map conv zipped
    where zipped = zip ["Id:         ","Path:       ","Title:      ","Authors:    ",
                        "Keywords:   ","Journal:    ","Volume:     ",
                        "Year:       ","Pages:      "] row 
          conv (key,val) | null val  = ""
                         | otherwise = key ++ val ++ "\n"

separateMulti :: ([String],[String]) -> (([String],[String]),([String],[String]))
separateMulti ps = worker [] [] [] [] ps
    where worker k1 v1 k2 v2 ([],[]) = ((k1,v1),(k2,v2))
          worker k1 v1 k2 v2 ((k:ks),(v:vs)) 
              | k `elem` ["Authors","Keywords","Pages"] = 
                            worker k1 v1 (k:k2) (v:v2) (ks,vs)
              | otherwise = worker (k:k1) (v:v1) k2 v2 (ks,vs)

separateMulti2 :: ([String],[String]) -> (([String],[String]),([String],[String]))
separateMulti2 ps = worker [] [] [] [] ps
    where worker k1 v1 k2 v2 ([],[]) = ((k1,v1),(k2,v2))
          worker k1 v1 k2 v2 ((k:ks),(v:vs)) 
              | k `elem` ["Authors","Keywords"] = 
                            worker k1 v1 (k:k2) (v:v2) (ks,vs)
              | otherwise = worker (k:k1) (v:v1) k2 v2 (ks,vs)

multiToList :: String -> [String]
multiToList [] = []
multiToList xs = let (v,rest) = break (=='|') xs
                 in if null rest 
                        then [v]
                        else v : (multiToList $ tail rest)

toFilter :: (String,[String]) -> Either String Filter
toFilter (f,vs) | null vs = Left "too few arguments"
                | otherwise = case f of
    "-f" -> Right $ File     $ concat $ intersperse " " vs
    "-t" -> Right $ Title    $ concat $ intersperse " " vs
    "-a" -> Right $ Authors  $ concat $ intersperse "|" vs
    "-k" -> Right $ Keywords $ concat $ intersperse "|" vs
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
                        else Left  $ "Invalid pages: " ++ (vs0 ++ "|" ++ vs1)
                _ -> Left "too many arguments to -p"     
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

updatePairs :: [String] -> ([String],[String]) -> ([String],[String])
updatePairs row new = ("Authors":"Keywords":k,na':nk':v)
    where ((k,v),ak) = separateMulti2 new
          (aa,ar)   = separateAddRem $ getValues "Authors" ak 
          (ka,kr)   = separateAddRem $ getValues "Keywords" ak
          na        = aa ++ (newVals (multiToList (row!!3)) ar aa)
          nk        = ka ++ (newVals (multiToList (row!!4)) kr ka)  
          na'       = concat $ intersperse "|" na
          nk'       = concat $ intersperse "|" nk


newVals :: [String] -> [String] -> [String] -> [String]
newVals old rm add = (old \\ rm) \\ add

separateAddRem :: String -> ([String],[String])
separateAddRem str = worker [] [] $ multiToList str
    where worker add rm [] = (add,rm)
          worker add rm (x:xs) = if head x == '+'
                                     then worker ((tail x):add) rm xs
                                     else worker add (x:rm) xs 
