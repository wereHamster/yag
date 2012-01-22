module Git.Command.WriteTree (run) where

import Git.Index
import Git.Object.Tree

run :: [String] -> IO ()
run argv = do
    index <- loadIndex
    let tree = indexTree index
    putStrLn $ show tree
    return ()
