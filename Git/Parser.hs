{-# LANGUAGE OverloadedStrings #-}  
-- SM: This allows you to use string literals for 'S.ByteString's. The
-- corresponding 'IsString' class instance is imported from
-- "Data.ByteString.Char8".
module Git.Parser where

-- Internals modules are required only in very rare circumstances.
-- import           Data.ByteString.Internal  

import qualified Data.ByteString             as S
import qualified Data.ByteString.Lazy        as L
-- if your really only need ASCII strings, then the Char8 versions are what
-- you are looking for
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy.Char8  as L8
import           Data.Char (ord)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           Data.Word (Word8)

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AP (word8, take, takeWhile, takeTill)
import Data.Attoparsec.Char8 hiding (take)

import System.Locale

import Git.Utils
import Git.Hash
import Git.Identity

import Git.Object
import Git.Object.Blob
import Git.Object.Commit
import Git.Object.Tag
import Git.Object.Tree

-- SM: If you start your comment with  -- |  then Haddock will parse it as the
-- function documentation. See
-- <http://www.haskell.org/haddock/doc/html/ch03s08.html> for more
-- information. (BTW, sometimes the Haddock markup language feels a bit
-- underpowered.  I hope somebody (I) will do something about this.)

-- | A newline character, used as a delimiter and such.
newline = char '\n'

-- | Convert a 'Char' to a 'Word8' by truncating its Unicode codepoint.
c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Number encoded in octal, the mode in the tree is stored like that.
octal :: Parser Int
octal = S.foldl' step 0 <$> AP.takeWhile isOctalDigit where
    isOctalDigit a = 48 <= a && a <= 55
    step a w = a * 8 + fromIntegral (w - 48)

-- SM: As you noted in your TODO list: Use bytestrings everywhere. Linked
-- lists are just too slow for the sort of stuff you're doing.
--
-- BTW: Is the git format an ASCII or a Unicode format?

-- Convert a strict ByteString to a normal string. This function assumes that
-- the bytestring is ASCII encoded.
toString :: S.ByteString -> String
toString = S8.unpack

-- A null terminated string. We don't check the contents, just that it ends
-- with a null, and then convert it to a string.
nullString :: Parser String
nullString = toString <$> AP.takeTill (== 0) <* (AP.word8 0)

-- | Parse the remainder of the input as a single strict 'S.ByteString'.
takeRest :: Parser S.ByteString
takeRest = AP.takeWhile (const True)

-- Parse the remainder of the input as a string.
remString :: Parser String
remString = toString <$> takeRest <* endOfInput

-- Binary and hex encoded hashes.
binaryHash, stringHash :: Parser Hash
binaryHash = fromBinaryByteString <$> AP.take 20
stringHash = fromHexByteString <$> AP.take 40

-- The Tree, Commit and Tag objects are plaintext and start with header lines.
-- This is a parser for these header lines, it splits each line into a
-- (key, value) tuple.
header :: Parser (String, S.ByteString)
header = 
    ctor <$> key <* space <*> value
  where
    -- SM: I rather like the following style guide:
    --
    --   https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
    --
    -- I found that it results in very readable code.
    ctor k v  = (toString k, v)
    key   = AP.takeWhile (not . (== c2w ' '))
    value = AP.takeWhile (not . isEndOfLine) <* endOfLine

-- The identity is "name <email>".
identity :: Parser Identity
identity = ctor <$> name <*> email where
    ctor name email = Identity (strip $ toString name) (strip $ toString email)
    name  = AP.takeWhile (not . (== c2w '<')) <* char '<'
    email = AP.takeWhile (not . (== c2w '>')) <* char '>'

timeZone :: Parser TimeZone
timeZone = ctor <$> signed decimal where
    ctor offset = TimeZone (hours * 60 + minutes) False "" where
        hours = offset `div` 100; minutes = offset - hours * 100


-- Unix timestamp + offset
timestamp :: Parser ZonedTime
timestamp = ctor <$> decimal <* space <*> timeZone where
    ctor seconds zone = utcToZonedTime zone (posixSecondsToUTCTime $ realToFrac seconds)

-- Identity + ZonedTime, space separated, as it appears in header lines.
identTime :: Parser (Identity, ZonedTime)
identTime = (,) <$> identity <* space <*> timestamp



-- Parser: Blob
------------------------------------------------------------------------------
-- A blob is just a bunch of bytes. We need to convert the data into a lazy
-- bytestring, but that's it.
blobParser :: Parser Blob
blobParser = ctor <$> anything where
    ctor a = Git.Object.Blob.Blob $ L.fromChunks [a]
    anything = takeRest <* endOfInput



-- Parser: Commit
------------------------------------------------------------------------------
-- A commit consists of many headers, followed by a empty line, followed by
-- the commit message.
commitParser :: Parser Commit
commitParser = ctor <$> manyTill header newline <*> remString where
    ctor headers message = applyCommitHeaders commit headers where
        commit = emptyCommit { commitMessage = message }

-- Currently we don't handle the case when this function fails.
applyCommitAuthor, applyCommitCommitter :: Commit -> S.ByteString -> Commit
applyCommitAuthor commit hdr = case parseOnly identTime hdr of
    Right (ident, time) -> commit { commitAuthor = ident, commitAuthorDate = time }

applyCommitCommitter commit hdr = case parseOnly identTime hdr of
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



-- Parser: Tag
------------------------------------------------------------------------------
-- A tag is much like a commit, a bunch of headers, then the message.
tagParser :: Parser Tag
tagParser = ctor <$> manyTill header newline <*> remString where
    ctor headers message = applyTagHeaders tag headers where
        tag = emptyTag { tagMessage = message }

    message = takeRest <* endOfInput

applyTagTagger :: Tag -> S.ByteString -> Tag
applyTagTagger tag hdr = case parseOnly identTime hdr of
    Right (ident, time)-> tag { tagTagger = ident, tagTaggerDate = time }

-- Given a list of headers, apply them to the tag.
applyTagHeaders :: Tag -> [(String, S.ByteString)] -> Tag
applyTagHeaders = foldl applyHeader where
    applyHeader tag (key, value)
        | key == "object" =
            tag { tagObject = fromHexByteString value }
        | key == "type" =
            tag { tagObjectType = typeFromString $ toString value }
        | key == "tagger" =
            applyTagTagger tag value
        | otherwise = tag



-- Parser: Tree
------------------------------------------------------------------------------
-- The tree consists of many tree entries.
treeParser :: Parser Tree
treeParser = Git.Object.Tree.Tree <$> many1 treeEntry where
    treeEntry = Entry <$> (octal <* space) <*> nullString <*> binaryHash


data Object = GitBlob Blob | GitCommit Commit | GitTag Tag | GitTree Tree
    deriving (Show)

-- Return the proper parser for the given object type.
objectBuilder :: (Git.Object.Type, Int) -> Parser Object
objectBuilder (Git.Object.Blob,   length) = GitBlob   <$> blobParser
objectBuilder (Git.Object.Commit, length) = GitCommit <$> commitParser
objectBuilder (Git.Object.Tag,    length) = GitTag    <$> tagParser
objectBuilder (Git.Object.Tree,   length) = GitTree   <$> treeParser

gitObject :: Parser Object
gitObject = header >>= objectBuilder where
    header = (,) <$> (objectType <* space) <*> (decimal <* (char '\0'))


objectType :: Parser Git.Object.Type
objectType = blob <|> commit <|> tag <|> tree where
    blob   = Git.Object.Blob   <$ (string "blob")
    commit = Git.Object.Commit <$ (string "commit")
    tag    = Git.Object.Tag    <$ (string "tag")
    tree   = Git.Object.Tree   <$ (string "tree")


{-
commitData = S.pack $ Prelude.map c2w "tree b9c78fce14142eadb1515b433582e3e30899a3b8\nparent 9b9cb51d592a6217404806acdf7acd010eccc048\nauthor arst <qwfp> 3456 +0100\n\nmessage"
testParse = case parse commitParser commitData of
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
testTree = case parseOnly treeParser treeData of
    Left err -> error $ "fail " ++ show err
    Right tree -> tree
-}
