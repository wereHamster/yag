module Git.Command.Init (run) where

import System.Directory

templateDirectories :: [String]
templateDirectories = [
    ".git",
    ".git/objects",
    ".git/objects/info",
    ".git/objects/pack",
    ".git/hooks"
    ]

run :: [String] -> IO ()
run args = do
    putStrLn "Initializing repo"
    mapM (createDirectoryIfMissing True) templateDirectories
    return ()
