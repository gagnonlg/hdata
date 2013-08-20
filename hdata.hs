module Main where

import System.Environment
import Add
import Bookmark
import Citation
import Modify
import Remove
import Search
import Util
import View

help :: [String] -> IO ()
help argv = putStrLn $ usage Help

usageHelp = "usage: " ++ progName ++ " [operation] [id]\n\
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

version :: IO ()
version = putStrLn $ progName ++ " " ++ progVersion

main :: IO ()
main = do 
    argv <- getArgs
    if null argv 
    then 
      do 
        error $ "No operation specified ('" ++ progName ++ " help' for help)"
    else
      do
        let argv' = tail argv 
        case parseArg (head argv) of
            Right Add      -> add argv'
            Right Bookmark -> bookmark argv'
            Right Citation -> citation argv'
            Right Help     -> help argv'
            Right Modify   -> modify argv'
            Right Remove   -> remove argv'
            Right Search   -> search argv'
            Right View     -> view argv'
            Right Version  -> version
            Left msg       -> error msg

    

