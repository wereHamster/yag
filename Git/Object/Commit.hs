
module Git.Object.Commit where

import Data.ByteString.Internal
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import qualified Data.Attoparsec as AP (takeWhile)
import Data.Attoparsec.Char8 hiding (take)

import System.Locale
import Data.Time
import Data.Word
import Data.Char

import Git.Hash
import Git.Object
import Git.Identity

data Commit = Commit {
    commitTree :: Hash, commitParents :: [Hash],

    -- Each commit has an author and a committer. And dates for both.
    commitAuthor :: Identity,      commitCommitter :: Identity,
    commitAuthorDate :: UTCTime,   commitCommitterDate :: UTCTime,

    commitMessage :: String
} deriving (Eq)

emptyCommit :: Commit
emptyCommit = Git.Object.Commit.Commit nullHash [] defaultIdentity defaultIdentity time time "" where
    time = UTCTime (ModifiedJulianDay 0) 0


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



commitHash :: Commit -> Hash
commitHash commit = hashFromObject Git.Object.Commit commitData where
    commitData = L.pack $ map (fromIntegral . ord) $ show commit
