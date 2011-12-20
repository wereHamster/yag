
module Git.Object.Commit where

import qualified Data.ByteString.Lazy as L

import System.Locale
import Data.Time
import Data.Char

import Git.Hash
import Git.Object
import Git.Identity

data Commit = Commit {
    commitParents :: [Hash], commitTree :: Hash,

    -- Each commit has an author and a committer. And dates for both.
    commitAuthor :: Identity,       commitCommitter :: Identity,
    commitAuthorDate :: UTCTime,   commitCommitterDate :: UTCTime,

    commitMessage :: String
} deriving (Eq)


instance Show Commit where
    show commit = (unlines $ concat $ headers commit) ++ "\n" ++ (commitMessage commit)
        where
            headers commit   = map ($commit) [ tree, parents, author, committer ]
            timestamp        = formatTime defaultTimeLocale "%s %z"

            tree      commit = [ "tree " ++ (show $ commitTree commit) ]
            parents   commit = map (\x -> "parent " ++ (show x)) $ commitParents commit
            author    commit = [ "author " ++ (show $ commitAuthor commit) ++ " " ++ (timestamp $ commitAuthorDate commit) ]
            committer commit = [ "committer " ++ (show $ commitCommitter commit) ++ " " ++ (timestamp $ commitCommitterDate commit) ]


isRootCommit :: Commit -> Bool
isRootCommit commit = (length $ commitParents commit) == 0

commitMessageSubject :: Commit -> String
commitMessageSubject commit = head $ lines $ commitMessage commit


commitHash :: Commit -> Hash
commitHash commit = hashFromObject Git.Object.Commit commitData
    where
        commitData = L.pack $ map (fromIntegral . ord) $ show commit
