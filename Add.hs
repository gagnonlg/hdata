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
          | Issue String
          | Date String
          | Pages String
          deriving (Eq,Show)

isFlag :: String -> Bool
isFlag f = f `elem` ["-f","-p","-t","-j","-i","-d","-v","-a","-k"]

isPathFlag :: Flag -> Bool
isPathFlag f = case f of
    Path _ -> True
    _     -> False

add :: [String] -> IO ()
add [] = error $ "add: no arguments specified ('" ++ progName ++ " add help' for help)"
add argv = if isHelp $ head argv 
    then do putStrLn usageAdd 
    else do 
        case parseFlags argv of
            Left  msg   -> error $ "add: " ++ msg
            Right flags -> do checkFile flags 
                              putStrLn (flagsToString flags)
                              runSQL (buildSQL flags)

buildSQL :: [Flag] -> String
buildSQL flags = buildSQL' ("INSERT INTO " ++ tableName ++ " (") "VALUES(" flags
    where buildSQL' t1 t2 [] = ((init t1) ++ ") ") ++ (init(t2) ++ ");")
          buildSQL' t1 t2 (f:fs) = buildSQL' (t1++key++",") (t2++"'"++value++"',") fs
              where (key,val) = break (==' ') $ show f
                    value     = filter (/= '\"') (tail val)

checkFile :: [Flag] -> IO ()
checkFile fs = case filter isPathFlag fs of
    []     -> return ()
    ((Path p):_)  -> do exists <- doesFileExist p
                        if exists 
                            then do return ()
                            else do error $ "File does not exists: " ++ p   

isDate :: String -> Bool
isDate str = and [and (map isDigit str), (size == 4) || (size == 6) || (size == 8)]
    where size = length str

isPages :: String -> Bool
isPages str = and (map isDigit (pf ++ pt))
    where (pf,pt') = break (=='-') str
          pt       = if null pt' then "0" else tail pt'

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
    if isFlag x1 
        then Left "too few argument"
        else case x0 of
                 "-f" -> Right $ Path    x1
                 "-t" -> Right $ Title   x1
                 "-j" -> Right $ Journal x1
                 "-v" -> Right $ Volume  x1
                 "-i" -> Right $ Issue   x1
                 "-d" -> if isDate x1 
                             then Right $ Date x1 
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
            \    -i <issue>\n\
            \    -d <date> : <yyyy> OR <mmyyyy> OR <ddmmyyyy>\n\
            \    -p <page-from>-<page-to>"
