module Add (
    add,
    usageAdd
) where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import Util

data Flag = Path String
          | Title String
          | Authors String
          | Keywords String
          | Journal String
          | Issue String
          | Date String
          | Pages String
          deriving (Show)

isFlag :: String -> Bool
isFlag f = f `elem` ["-f","-p","-t","-j","-i","-d","-a","-k"]

add :: [String] -> IO ()
add [] = error $ "add: no arguments specified ('" ++ progName ++ " add help' for help)"
add argv = if isHelp $ head argv 
    then do putStrLn usageAdd 
    else do 
        case parseFlags argv of
            Left  msg   -> error $ "add: " ++ msg
            Right flags -> putStr $ flagsToString flags

flagsToString :: [Flag] -> String
flagsToString xs = foldl' step [] xs
    where step ys x = show x ++ "\n" ++ ys

parseFlags :: [String] -> Either String [Flag]
parseFlags argv = parseFlags' [] argv
    where parseFlags' _  (x:[]) = Left "too few arguments"
          parseFlags' fs []     = Right fs
          parseFlags' fs xs     = 
              let flag = getFlag xs
              in case flag of
                  Left  msg  -> Left msg
                  Right f    -> parseFlags' (f:fs) (dropWhile (not . isFlag) (tail xs))

getFlag :: [String] -> Either String Flag
getFlag x@(x0:x1:_) = 
    let flag = case x0 of
                   "-f" -> Path    x1
                   "-t" -> Title   x1
                   "-j" -> Journal x1
                   "-i" -> Issue   x1
                   "-d" -> Date    x1
                   "-p" -> Pages   x1
                   "-k" -> Keywords $ getValues $ tail  x
                   "-a" -> Authors  $ getValues $ tail  x
    in if isFlag x1 
           then Left "too few argument"
           else Right flag 

getValues :: [String] -> String
getValues argv = intercalate "|" $ takeWhile (not . isFlag) argv 

opendb :: IO Connection
opendb = do
    conn <- connectSqlite3 dbName
    run conn  ("CREATE TABLE " ++ tableName ++ "(id       INTEGER PRIMARY KEY,\
                                              \ path     VARCHAR(1000),\
                                              \ title    VARCHAR(1000),\
                                              \ authors  VARCHAR(1000),\
                                              \ keywords VARCHAR(1000),\
                                              \ journal  VARCHAR(1000),\
                                              \ volume   VARCHAR(1000),\
                                              \ issue    VARCHAR(1000),\
                                              \ date     VARCHAR(1000),\
                                              \ pages    VARCHAR(1000));") []
    commit conn
    return conn

usageAdd :: String
usageAdd = "usage: " ++ progName ++ " add <filters>\n\
            \filters:\n\
            \    -f <file>\n\
            \    -t <title>\n\
            \    -a <author1 [author2] ...>\n\
            \    -k <keyword1 [keyword2] ...>\n\
            \    -j <journal>\n\
            \    -i <issue>\n\
            \    -d <date>\n\
            \    -p <page-from>-<page-to>"
