module Util (
    Operation (..),
    createdb,
    isHelp,
    parseArg,
    dbName,
    opendb,
    progName,
    progVersion,
    tableName,
    usage
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Environment

dbName = progName ++ ".db"
progName = "hdata"
progVersion = "0.0"
tableName = "mainTable"

data Operation = Add
               | Bookmark
               | Citation
               | Help
               | Modify
               | Remove
               | Search
               | View
               | Version
               deriving (Show)

createdb :: Connection -> IO Connection
createdb conn = do run conn ("CREATE TABLE " ++ tableName ++ "(id       INTEGER PRIMARY KEY,\
                                                        \ Path     VARCHAR(1000),\
                                                        \ Title    VARCHAR(1000),\
                                                        \ Authors  VARCHAR(1000),\
                                                        \ Keywords VARCHAR(1000),\
                                                        \ Journal  VARCHAR(1000),\
                                                        \ Volume   VARCHAR(1000),\
                                                        \ Issue    VARCHAR(1000),\
                                                        \ Date     VARCHAR(1000),\
                                                        \ Pages    VARCHAR(1000));") []
                   commit conn
                   return conn

isHelp :: String -> Bool
isHelp str = str `elem` ["-h","help","--help"]

opendb :: IO Connection
opendb = do
    conn <- connectSqlite3 dbName
    tables <- getTables conn
    if not (tableName `elem` tables)
        then do createdb conn
        else do return conn

usage :: Operation -> String
usage op = show op ++ " not yet implemented" 



parseArg :: String -> Either String Operation
parseArg op = case op of  
    "add"      -> Right Add
    "bookmark" -> Right Bookmark
    "citation" -> Right Citation
    "help"     -> Right Help
    "-h"       -> Right Help
    "--help"   -> Right Help
    "modify"   -> Right Modify
    "remove"   -> Right Remove
    "search"   -> Right Search
    "view"     -> Right View
    "version"  -> Right Version
    _          -> Left $ "Invalid argument: " ++ op 


