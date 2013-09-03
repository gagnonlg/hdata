{-
    Tools/SQL.hs

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

module Tools.SQL (
    buildSQLAdd,
    getEntry,
    runSQL
) where

import Data.List (intersperse)
import Database.HDBC
import Database.HDBC.Sqlite3

import Tools.Constants

buildSQLAdd :: ([String],[String]) -> String
buildSQLAdd (ks,vs) = 
    "INSERT INTO " ++ tableName ++ " (" ++ keys ++ ") VALUES(" ++ vals ++ ");"
    where keys = concat $ intersperse "," ks
          vals = "'" ++ (concat (intersperse "','" vs)) ++ "'"

createdb :: Connection -> IO Connection
createdb conn = do run conn ("CREATE TABLE " ++ tableName ++ "(id       INTEGER PRIMARY KEY,\
                                                        \ File     VARCHAR(1000),\
                                                        \ Title    VARCHAR(1000),\
                                                        \ Authors  VARCHAR(1000),\
                                                        \ Keywords VARCHAR(1000),\
                                                        \ Journal  VARCHAR(1000),\
                                                        \ Volume   VARCHAR(1000),\
                                                        \ Year     VARCHAR(1000),\
                                                        \ Pages    VARCHAR(1000),\
                                                        \ Bookmarked VARCHAR(1000));") []
                   commit conn
                   return conn

opendb :: IO Connection
opendb = do
    conn <- connectSqlite3 dbName
    tables <- getTables conn
    if not (tableName `elem` tables)
        then do createdb conn
        else do return conn

runSQL :: String -> IO ()
runSQL sql = do
    db <- opendb
    run db sql []
    commit db
    disconnect db
    return ()

getFromDB :: String -> IO [[SqlValue]]
getFromDB sql = do 
    db <- opendb
    result <- quickQuery' db sql []
    disconnect db
    return result

getEntry :: Int -> IO (Either String [String])
getEntry id = do row <- retrieveSqlValues id 
                 case row of
                    Left msg      -> return $ Left msg
                    Right sqlVals -> return $ Right $ map fromSqlToString sqlVals

fromSqlToString :: SqlValue -> String
fromSqlToString SqlNull = ""
fromSqlToString value = fromSql value

retrieveSqlValues :: Int -> IO (Either String [SqlValue])
retrieveSqlValues id = do 
    let sql = "SELECT * FROM " ++ tableName ++ " WHERE id = " ++ (show id)
    result <- getFromDB sql 
    case result of
        []    -> return $ Left $ "entry no. " ++ (show id) ++ " doesn't exist"
        [val] -> return $ Right val
