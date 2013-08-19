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

parseArgs :: [String] -> Maybe Operation
parseArgs xs = if null xs then Just Help else parseArg $ head xs

parseArg :: String -> Maybe Operation 
parseArg op = case op of  
    "add"      -> Just Add
    "bookmark" -> Just Bookmark
    "citation" -> Just Citation
    "help"     -> Just Help
    "-h"       -> Just Help
    "--help"   -> Just Help
    "modify"   -> Just Modify
    "remove"   -> Just Remove
    "search"   -> Just Search
    "view"     -> Just View
    "version"  -> Just Version
    _          -> Nothing

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
        Nothing -> error $ "Invalid argument: " ++  (show $ head argv)
        Just op -> case op of 
            Add      -> add argv
            Bookmark -> bookmark argv
            Citation -> citation argv
            Help     -> help argv
            Modify   -> modify argv
            Remove   -> remove argv
            Search   -> search argv
            View     -> view argv
            Version  -> version

    

