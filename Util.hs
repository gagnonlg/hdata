module Util (
    Operation (..),
    isHelp,
    parseArg,
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

isHelp :: String -> Bool
isHelp str = str `elem` ["-h","help","--help"]

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


