
module Git.Parser where

import Data.ByteString.Internal
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AP (word8, take, takeWhile, takeTill)
import Data.Attoparsec.Char8 hiding (take)

import System.Locale
import Data.Time
import Data.Time.Clock.POSIX

import Git.Utils
import Git.Hash
import Git.Identity

import Git.Object
import Git.Object.Blob
import Git.Object.Commit
import Git.Object.Tag
import Git.Object.Tree

-- A newline character, used as a delimiter and such.
newline = char '\n'

-- Number encoded in octal, the mode in the tree is stored like that.
octal :: Parser Int
octal = S.foldl' step 0 <$> AP.takeWhile isOctalDigit where
    isOctalDigit a = 48 <= a && a <= 55
    step a w = a * 8 + fromIntegral (w - 48)

-- Convert a strict ByteString to a normal string. This function assumes that
-- the bytestring is ASCII encoded.
toString :: S.ByteString -> String
toString x = map w2c $ S.unpack x

-- A null terminated string. We don't check the contents, just that it ends
-- with a null, and then convert it to a string.
nullString :: Parser String
nullString = toString <$> AP.takeTill (== 0) <* (AP.word8 0)

-- Parse the remainder of the input as a string.
remString :: Parser String
remString = toString <$> AP.takeWhile (const True) <* endOfInput

-- Binary and hex encoded hashes.
binaryHash, stringHash :: Parser Hash
binaryHash = hashFromBinary <$> AP.take 20
stringHash = hashFromString <$> AP.take 40

-- The Tree, Commit and Tag objects are plaintext and start with header lines.
-- This is a parser for these header lines, it splits each line into a
-- (key, value) tuple.
header :: Parser (String, S.ByteString)
header = ctor <$> key <* space <*> value where
    ctor k v  = (toString k, v)
    key   = AP.takeWhile (not . (== c2w ' '))
    value = AP.takeWhile (not . isEndOfLine) <* endOfLine

-- The identity is "name <email>".
identity :: Parser Identity
identity = ctor <$> name <*> email where
    ctor name email = Identity (strip $ toString name) (strip $ toString email)
    name  = AP.takeWhile (not . (== c2w '<')) <* char '<'
    email = AP.takeWhile (not . (== c2w '>')) <* char '>'

-- Unix timestamp + offset. The offset is currently ignored.
timestamp :: Parser UTCTime
timestamp = ctor <$> decimal <* space <* signed decimal where
    ctor seconds = posixSecondsToUTCTime $ realToFrac seconds

-- Identity + UTCTime, space separated, as it appears in header lines.
identTime :: Parser (Identity, UTCTime)
identTime = (,) <$> identity <* space <*> timestamp



-- Parser: Blob
------------------------------------------------------------------------------
-- A blob is just a bunch of bytes. We need to convert the data into a lazy
-- bytestring, but that's it.
parseBlob :: Parser Blob
parseBlob = ctor <$> anything where
    ctor a = Git.Object.Blob.Blob $ L.fromChunks [a]
    anything = AP.takeWhile (const True) <* endOfInput



-- Parser: Commit
------------------------------------------------------------------------------
-- A commit consists of many headers, followed by a empty line, followed by
-- the commit message.
parseCommit :: Parser Commit
parseCommit = ctor <$> manyTill header newline <*> remString where
    ctor headers message = applyCommitHeaders commit headers where
        commit = emptyCommit { commitMessage = message }

    message = AP.takeWhile (const True) <* endOfInput

-- Currently we don't handle the case when this function fails.
applyCommitAuthor, applyCommitCommitter :: Commit -> S.ByteString -> Commit
applyCommitAuthor commit hdr = case parseOnly identTime hdr of
    Right (ident, time) -> commit { commitAuthor = ident, commitAuthorDate = time }

applyCommitCommitter commit hdr = case parseOnly identTime hdr of
    Right (ident, time) -> commit { commitCommitter = ident, commitCommitterDate = time }

-- Given a list of headers, apply them to the commit.
applyCommitHeaders :: Commit -> [(String, S.ByteString)] -> Commit
applyCommitHeaders = foldl applyHeader where
    applyHeader commit (key, value)
        | key == "tree" =
            commit { commitTree = hashFromHexStrict value }
        | key == "parent" =
            commit { commitParents = (commitParents commit) ++ [(hashFromHexStrict value)] }
        | key == "author" =
            applyCommitAuthor commit value
        | key == "committer" =
            applyCommitCommitter commit value
        | otherwise = commit



-- Parser: Tag
------------------------------------------------------------------------------
-- A tag is much like a commit, a bunch of headers, then the message.
parseTag :: Parser Tag
parseTag = ctor <$> manyTill header newline <*> remString where
    ctor headers message = applyTagHeaders tag headers where
        tag = emptyTag { tagMessage = message }

    message = AP.takeWhile (const True) <* endOfInput

applyTagTagger :: Tag -> S.ByteString -> Tag
applyTagTagger tag hdr = case parseOnly identTime hdr of
    Right (ident, time)-> tag { tagTagger = ident, tagTaggerDate = time }

-- Given a list of headers, apply them to the tag.
applyTagHeaders :: Tag -> [(String, S.ByteString)] -> Tag
applyTagHeaders = foldl applyHeader where
    applyHeader tag (key, value)
        | key == "object" =
            tag { tagObject = hashFromHexStrict value }
        | key == "type" =
            tag { tagObjectType = typeFromString $ toString value }
        | key == "tagger" =
            applyTagTagger tag value
        | otherwise = tag



-- Parser: Tree
------------------------------------------------------------------------------
-- The tree consists of many tree entries.
parseTree :: Parser Tree
parseTree = Git.Object.Tree.Tree <$> many1 treeEntry where
    treeEntry = Entry <$> (octal <* space) <*> nullString <*> binaryHash


commitData = S.pack $ Prelude.map c2w "tree b9c78fce14142eadb1515b433582e3e30899a3b8\nparent 9b9cb51d592a6217404806acdf7acd010eccc048\nauthor arst <qwfp> 3456 +0100\n\nmessage"
testParse = case parse parseCommit commitData of
    Fail err a b -> "fail " ++ show err
    Partial a -> case a S.empty of
        Fail err a b -> show err ++ show a ++ show b
        Done a b -> "done: " ++ show b
    Done a b -> "done"

identityData = S.pack $ Prelude.map c2w "Tomas Carnecky <tomas.carnecky@gmail.com> 8765678987 +0100"
testIdentity = case parse identTime identityData of
    Fail err a b -> "fail " ++ show err
    Partial a -> case a S.empty of
        Fail err a b -> show err ++ show a ++ show b
        Done a b -> "done after partial: " ++ show b
    Done a b -> "done " ++ show b

treeData = S.pack $ Prelude.map c2w $ "100644 bar\0\255\1\2\3\4\5\6\7\8\9\0\1\2\3\4\5\6\7\8\9" ++ "100755 foo\0\0\1\2\1\2\1\2\1\2\1\2\1\2\1\2\1\2\1\2\1"
testTree = case parseOnly parseTree treeData of
    Left err -> error $ "fail " ++ show err
    Right tree -> tree
