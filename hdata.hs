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
             \    version\n"   

usage op = show op ++ " not yet implemented" 

parseArgs :: [String] -> Either String Operation
parseArgs xs = if null xs then Right Help else parseArg $ head xs

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

add :: [String] -> IO ()
add argv = putStrLn $ usage Add

bookmark :: [String] -> IO ()
bookmark argv = putStrLn $ usage Bookmark

citation :: [String] -> IO ()
citation argv = putStrLn $ usage Citation

help :: [String] -> IO ()
help argv = putStrLn $ usage Help

modify :: [String] -> IO ()
modify argv = putStrLn $ usage Modify

remove :: [String] -> IO ()
remove argv = putStrLn $ usage Remove

search :: [String] -> IO ()
search argv = putStrLn $ usage Search

view :: [String] -> IO ()
view argv = putStrLn $ usage View

version :: IO ()
version = putStrLn $ progName ++ " " ++ progVersion

main :: IO ()
main = do 
    argv <- getArgs
    case parseArgs argv of
        Right Add      -> add argv
        Right Bookmark -> bookmark argv
        Right Citation -> citation argv
        Right Help     -> help argv
        Right Modify   -> modify argv
        Right Remove   -> remove argv
        Right Search   -> search argv
        Right View     -> view argv
        Right Version  -> version
        Left msg       -> error msg

    

