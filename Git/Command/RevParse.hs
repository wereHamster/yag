module Git.Command.RevParse (run) where

import Data.Maybe

import Git.Repository
import Git.Revision

run []     = return ()
run (x:xs) = do
    repo <- (findRepository ".") >>= return . fromJust
    rev <- resolveRevision repo $ fromJust . parseRevision $ x
    putStrLn $ show $ fromJust rev
    return ()
