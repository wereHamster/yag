
module Git.Traverse where

import Control.Monad
import Data.List
import Data.Maybe

import Git.Hash
import Git.Parser
import Git.Object.Commit
import Git.Repository

printCommit :: Commit -> IO ()
printCommit = putStrLn . onelineCommit

printObject :: Object -> IO ()
printObject (GitCommit commit) = printCommit commit

onlyCommits :: [Object] -> [Commit]
onlyCommits [] = []
onlyCommits ((GitCommit x):xs) = x : (onlyCommits xs)
onlyCommits (x:xs) = onlyCommits xs

rejectCommits :: [Hash] -> [Commit] -> [Commit]
rejectCommits [] xs = xs
rejectCommits _ [] = []
rejectCommits hashes all@(x:xs) = case find (== commitHash x) hashes of
    Just c -> rejectCommits hashes xs
    Nothing -> x : (rejectCommits hashes xs)

parents :: [Commit] -> [Hash]
parents commits = concat $ map commitParents commits

-- Maybe this is not the smartest way to do it. But what the hell, this works.
-- The idea here is that you specify the upper and lower bounds of your list,
-- in terms of hashes. And you get a list of Commits in between.
revList :: Repository -> [Hash] -> [Hash] -> IO [Commit]
revList _ _ [] = do return []
revList repo lower upper = do
    objects <- mapM (loadObject repo) upper
    let commits = rejectCommits lower $ onlyCommits objects
    liftM2 (++) (return commits) (revList repo lower (parents $ commits))
