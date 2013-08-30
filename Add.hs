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
import Util

data Flag = Path String
          | Title String
          | Authors String
          | Keywords String
          | Journal String
          | Volume String
          | Year String
          | Pages String
          deriving (Eq,Show)

isFlag :: String -> Bool
isFlag f = f `elem` ["-f","-p","-t","-j","-y","-v","-a","-k"]

isPathFlag :: Flag -> Bool
isPathFlag f = case f of
    Path _ -> True
    _     -> False

areEqual f1 f2 = f1' == f2' 
    where (f1',_) = break (==' ') $ show f1
          (f2',_) = break (==' ') $ show f2

add :: [String] -> IO ()
add [] = error $ "add: no arguments specified ('" ++ progName ++ " add help' for help)"
add argv = if isHelp $ head argv 
    then do putStrLn usageAdd 
    else do 
        case parseFlags argv of
            Left  msg   -> error $ "add: " ++ msg
            Right flags -> do checkDuplicates flags
                              checkFile flags 
                              putStrLn (flagsToString flags)
                              runSQL (buildSQL flags)

buildSQL :: [Flag] -> String
buildSQL flags = buildSQL' ("INSERT INTO " ++ tableName ++ " (") "VALUES(" flags
    where buildSQL' t1 t2 [] = ((init t1) ++ ") ") ++ (init(t2) ++ ");")
          buildSQL' t1 t2 (f:fs) = buildSQL' (t1++key++",") (t2++"'"++value++"',") fs
              where (key,val) = break (==' ') $ show f
                    value     = filter (/= '\"') (tail val)

checkDuplicates :: [Flag] -> IO () 
checkDuplicates (f:[]) = return ()
checkDuplicates (f:fs) = if or $ map (areEqual f) fs
                             then do let f' = fst $ break (==' ') $ show f
                                     error "add: duplicate arguments" 
                             else do checkDuplicates fs


checkFile :: [Flag] -> IO ()
checkFile fs = case filter isPathFlag fs of
    []     -> return ()
    ((Path p):_)  -> do exists <- doesFileExist p
                        if exists 
                            then do return ()
                            else do error $ "File does not exists: " ++ p   

isYear :: String -> Bool
isYear str = and [and (map isDigit str), (length str == 4)]

isPages :: String -> Bool
isPages str = and (map isDigit (pf ++ pt))
    where (pf,pt') = break (=='-') str
          pt       = if null pt' then "0" else tail pt'

flagsToString :: [Flag] -> String
flagsToString xs = foldl' step [] xs
    where step ys x = show x ++ "\n" ++ ys

parseFlags :: [String] -> Either String [Flag]
parseFlags argv = parseFlags' [] argv
    where parseFlags' _  (x:[]) = if isFlag x 
                                    then Left "too few arguments"
                                    else Left $ "Invalid argument: " ++ x
          parseFlags' fs []     = Right fs
          parseFlags' fs xs     = 
              let flag = getFlag xs
              in case flag of
                  Left  msg  -> Left msg
                  Right f    -> parseFlags' (f:fs) (dropWhile (not . isFlag) (tail xs))

getFlag :: [String] -> Either String Flag
getFlag x@(x0:x1:_) =
    if isFlag x1 
        then Left "too few argument"
        else case x0 of
                 "-f" -> Right $ Path    x1
                 "-t" -> Right $ Title   x1
                 "-j" -> Right $ Journal x1
                 "-v" -> Right $ Volume  x1
                 "-y" -> if isYear x1 
                             then Right $ Year x1 
                             else Left $ "Invalid date: " ++ x1 ++ " ('" ++ progName ++ "\
                                         \ add help' for help)"
                 "-p" -> if isPages x1 
                             then Right $ Pages x1
                             else Left $ "Invalid pages: " ++ x1 ++ " ('" ++ progName ++ "\
                                         \ add help' for help)"
                 "-k" -> Right $ Keywords $ getValues $ tail  x
                 "-a" -> Right $ Authors  $ getValues $ tail  x
                 _    -> Left $ "Invalid argument: " ++ x0

getValues :: [String] -> String
getValues argv = intercalate "/" $ takeWhile (not . isFlag) argv 


runSQL :: String -> IO ()
runSQL sql = do
    db <- opendb
    run db sql []
    commit db
    disconnect db
    return ()

usageAdd :: String
usageAdd = "usage: " ++ progName ++ " add <filters>\n\
            \filters:\n\
            \    -f <file>\n\
            \    -t <title>\n\
            \    -a <author1 [author2] ...>\n\
            \    -k <keyword1 [keyword2] ...>\n\
            \    -j <journal>\n\
            \    -y <year> : <yyyy>\n\
            \    -p <page-from>-<page-to>"
