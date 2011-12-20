
import Data.Time

import Git.Object
import Git.Object.Commit
import Git.Object.Tree
import Git.Object.Tag
import Git.Hash
import Git.Identity

now = UTCTime (ModifiedJulianDay 550000) 0

tree = Git.Object.Tree.Tree [
        Entry 0o100644 Git.Object.Blob emptyTreeHash "path.c",
        Entry 0o100644 Git.Object.Tree emptyTreeHash "directory"
    ]

commit = Git.Object.Commit.Commit [] emptyTreeHash defaultIdentity defaultIdentity now now "Commit subject"

tag = Git.Object.Tag.Tag emptyTreeHash Git.Object.Tree defaultIdentity now "v1.0" "Message"
