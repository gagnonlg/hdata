{- Config.h -- configuration file for hddb -}

module Config (
    defaultViewer
) where

{-
    this is the default viewer that is launched by
    the 'view' command
-}
defaultViewer :: String
defaultViewer = "vim"
