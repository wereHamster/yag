
module Git.Object.Commit (

    -- The 'Commit' type
    Commit(..),

    emptyCommit, commitHash

) where

import qualified Data.ByteString.Lazy as L

import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Data.Word
import Data.Char
import Data.List

import Git.Hash
import Git.Object
import Git.Identity

data Commit = Commit {
    commitTree :: Hash, commitParents :: [Hash],

    -- Each commit has an author and a committer. And dates for both.
    commitAuthor :: Identity,        commitCommitter :: Identity,
    commitAuthorDate :: ZonedTime,   commitCommitterDate :: ZonedTime,

    commitMessage :: String
}

emptyCommit :: Commit
emptyCommit = Git.Object.Commit.Commit nullHash [] defaultIdentity defaultIdentity time time "" where
    time = utcToZonedTime utc $ posixSecondsToUTCTime $ realToFrac 0

commitHash :: Commit -> Hash
commitHash commit = fromObject Git.Object.Commit commitData where
    commitData = L.pack $ map (fromIntegral . ord) $ show commit


onelineCommit :: Commit -> String
onelineCommit commit = concat $ intersperse " " [ hash, subject, date ] where
    hash = abbrev 7 $ commitHash commit
    subject = commitMessageSubject commit
    date = formatTime defaultTimeLocale "(%Y-%m-%d %H:%M)" (commitAuthorDate commit)

instance Show Commit where
    show commit = (unlines $ concat $ headers commit) ++ "\n" ++ (commitMessage commit) where
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
