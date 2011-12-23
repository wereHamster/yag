module Git.Command.WriteTree (run) where

import Git.Object.Tree

run :: [String] -> IO ()
run argv = do
    putStrLn $ show emptyTreeHash
    return ()
