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
    createdb,
    opendb,
    runSQL
) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Tools.Constants

createdb :: Connection -> IO Connection
createdb conn = do run conn ("CREATE TABLE " ++ tableName ++ "(id       INTEGER PRIMARY KEY,\
                                                        \ Path     VARCHAR(1000),\
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



