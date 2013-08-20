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

version :: IO ()
version = putStrLn $ progName ++ " " ++ progVersion

main :: IO ()
main = do 
    argv <- getArgs
    let argv' = tail argv 
    case parseArgs argv of
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

    

