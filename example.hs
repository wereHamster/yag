
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Git.Object
import Git.Object.Commit
import Git.Object.Tree
import Git.Object.Tag
import Git.Hash
import Git.Identity

zone = TimeZone 60 False ""
now = utcToZonedTime zone $ posixSecondsToUTCTime $ realToFrac 237833672

tree = Git.Object.Tree.Tree [
        Entry 0o000644 "path.c" emptyTreeHash,
        Entry 0o000644 "directory" emptyTreeHash
    ]

commit = Git.Object.Commit.Commit (treeHash tree) [] defaultIdentity defaultIdentity now now "Commit subject"

tag = Git.Object.Tag.Tag emptyTreeHash Git.Object.Tree defaultIdentity now "v1.0" "Message"
