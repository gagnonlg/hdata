module Util (
    Operation (..),
    parseArg,
    parseArgs,
    progName,
    progVersion,
    usage
) where

import System.Environment

progName = "hdata"
progVersion = "0.0"

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

usage :: Operation -> String

usage Add = "usage: " ++ progName ++ " add <filters>\n\
            \filters:\n\
            \    -f <file>\n\
            \    -t <title>\n\
            \    -a <author1 [author2] ...>\n\
            \    -k <keyword1 [keyword2] ...>\n\
            \    -j <journal>\n\
            \    -i <issue>\n\
            \    -d <date>\n\
            \    -p <page-from>-<page-to>"

usage Help = "usage: " ++ progName ++ " [operation] [id]\n\
             \operations:\n\
             \    add                  <filters>     \n\
             \    bookmark [options]             [id]\n\    
             \    citation [options]             [id]\n\    
             \    help     [operation]               \n\    
             \    modify   [options]   <filters> <id>\n\    
             \    remove   [options]             <id>\n\    
             \    search   [options]   [filters] [id]\n\    
             \    view     [options]             <id>\n\    
             \    version"   

usage op = show op ++ " not yet implemented" 

parseArgs :: [String] -> Either String Operation
parseArgs [] = Left $ "No operation specified ('" ++ progName ++ " help' for help)" 
parseArgs xs = parseArg $ head xs

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


