
module Git.Object.Commit (

    -- The 'Commit' type
    Commit,

    commitMessage, commitTree, commitAuthor, commitAuthorDate, commitCommitter, commitCommitterDate, commitParents,

    emptyCommit, commitHash, commitParser

) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AP (Parser, take, manyTill, parseOnly)

import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Data.Word
import Data.Char
import Data.List

import Git.Hash
import Git.Parser
import Git.Identity

data Commit = Commit {
    commitTree :: Hash, commitParents :: [Hash],

    -- Each commit has an author and a committer. And dates for both.
    commitAuthor :: Identity,        commitCommitter :: Identity,
    commitAuthorDate :: ZonedTime,   commitCommitterDate :: ZonedTime,

    commitMessage :: String
}

emptyCommit :: Commit
emptyCommit = Commit nullHash [] defaultIdentity defaultIdentity time time "" where
    time = utcToZonedTime utc $ posixSecondsToUTCTime $ realToFrac 0

commitHash :: Commit -> Hash
commitHash commit = fromObject "commit" commitData where
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


-- * Parser

commitParser :: AP.Parser Commit
commitParser = ctor <$> AP.manyTill header newline <*> remString where
    ctor headers message = applyCommitHeaders commit headers where
        commit = emptyCommit { commitMessage = message }

-- Currently we don't handle the case when this function fails.
applyCommitAuthor, applyCommitCommitter :: Commit -> S.ByteString -> Commit
applyCommitAuthor commit hdr = case AP.parseOnly identTime hdr of
    Right (ident, time) -> commit { commitAuthor = ident, commitAuthorDate = time }

applyCommitCommitter commit hdr = case AP.parseOnly identTime hdr of
    Right (ident, time) -> commit { commitCommitter = ident, commitCommitterDate = time }

-- Given a list of headers, apply them to the commit.
applyCommitHeaders :: Commit -> [(String, S.ByteString)] -> Commit
applyCommitHeaders = 
    foldl applyHeader 
  where
    applyHeader commit (key, value)
        | key == "tree" =
            commit { commitTree = fromHexByteString value }
        | key == "parent" =
            commit { commitParents = (commitParents commit) ++ [(fromHexByteString value)] }
        | key == "author" =
            applyCommitAuthor commit value
        | key == "committer" =
            applyCommitCommitter commit value
        | otherwise = commit -- SM: shouldn't this be an error or a warning at least?

