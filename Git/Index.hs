module Git.Index (

    Index(..),

    loadIndex, indexTree

) where

import System.IO
import System.FilePath
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Control.Applicative
import Data.Attoparsec
import qualified Data.Attoparsec.ByteString as AP (take, skip, takeTill)
import Data.Attoparsec.Binary

import Data.ByteString.Internal (c2w, w2c)
import Data.Bits
import Data.Word
import Data.List
import Text.Printf
import System.Posix.Types (EpochTime)

import Git.Hash
import Git.Object.Tree
import Git.Object.Blob


-- | The index is a list of entries and optional extensions. We currently
-- ignore the extensions.
data Index = Index { indexEntries :: [IndexEntry] }
    deriving Show

-- | An IndexEntry is a single entry in an Index, representing a single
-- file along with its hash, mode, as well as stat()-related info used to
-- decide whether an on-disk file has changed.
data IndexEntry = IndexEntry {
    hash :: Hash, name :: String, flags :: Int,

    -- These attributes are currently not used for anythign.
    ctime :: EpochTime, mtime :: EpochTime,
    dev :: Int, ino :: Int, mode :: Int, uid :: Int, gid :: Int, size :: Int
} deriving Show



-- * Parser

-- | The index consists of a header, followed by a number of entries, followed
-- by a number of extensions. We currently ignore any extensions.
indexParser :: Parser Index
indexParser = do
    num <- header; entries <- sequence $ replicate num entry
    return Index { indexEntries = entries  }

-- | The index header starts with "DIRE", followed by the version number
-- (currently 2), followed by the number of entries in the index.
header :: Parser Int
header = fromIntegral <$ word32be 0x44495243 <* word32be 2 <*> anyWord32be

-- | sec + nsec, where nsec always appears to be zero.
cacheTime :: Parser EpochTime
cacheTime = fromIntegral <$> anyWord32be <* word32be 0

entry :: Parser IndexEntry
entry = do
    ctime <- cacheTime
    mtime <- cacheTime
    dev   <- anyWord32be
    ino   <- anyWord32be
    mode  <- anyWord32be
    uid   <- anyWord32be
    gid   <- anyWord32be
    size  <- anyWord32be
    hash  <- binaryHash
    flagsword <- anyWord16be
    let namemask = 0xFFF
    let namelen = flagsword .&. namemask
    namebytes <-
        if namelen == namemask
            then fail "long name in index"
            else AP.take (fromIntegral namelen)
    -- TODO: parse flags.
    let flags = fromIntegral $ flagsword `shiftR` 12
    -- Need to read remaining pad bytes.
    --   (offsetof(struct cache_entry,name) + (len) + 8) & ~7
    -- It appears to waste 8 bytes if we're at an even multiple of 8?
    let padbytes = 8 - ((8+8+6*4+20+2+namelen) `mod` 8)
    AP.take $ fromIntegral padbytes
    let name = map w2c $ B.unpack namebytes
    return $ IndexEntry {
        ctime=ctime, mtime=mtime,
        dev = fromIntegral dev,
        ino = fromIntegral ino,
        mode = fromIntegral mode,
        uid = fromIntegral uid,
        gid = fromIntegral gid,
        size = fromIntegral size,
        hash=hash, flags=flags, name=name
    }

-- | Load and parse the git index file.
loadIndex :: IO Index
loadIndex = do
    handle <- openFile ".git/index" ReadMode
    hSetBinaryMode handle True
    contents <- B.hGetContents handle

    -- Last 20 bytes are SHA1, we ignore that.
    let raw = B.take (B.length contents - 20) contents
    case parseOnly indexParser raw of
        Right a -> return a
        otherwise -> error "Failed to parse the index"


-- This down below here is ugly. Don't look at it. It works sortof though, but
-- is probably not the fastest due to the heavy use of length, !! etc.

buildIndexTree :: Index -> String -> Tree -> [(String, Maybe IndexEntry)] -> Tree
buildIndexTree index prefix tree entries =
    foldl buildTree tree entries
  where
    buildTree tree entry = tree { treeEntries = entries } where
        entries = treeEntries tree ++ [toTreeEntry index prefix entry]


-- | Build a tree out of entries in the index with the given prefix.
indexTreePrefix :: Index -> String -> Tree
indexTreePrefix index prefix = do
    let entries = indexEntries index
    let l0 = filter (\x -> prefix `isPrefixOf` (name x)) entries
    let l1 = indexEntriesAt l0 prefix
    let l2 = nubBy (\x y -> (fst x) == (fst y)) l1
    buildIndexTree index prefix emptyTree l2

indexEntriesAt :: [IndexEntry] -> String -> [(String, Maybe IndexEntry)]
indexEntriesAt entries prefix = fmap f entries where
    f x = if '/' `elem` (drop (length prefix) (name x))
        then (init $ nameComponents !! prefixLength, Nothing)
        else (nameComponents !! prefixLength, Just x)
        where
            prefixComponents = splitPath prefix
            prefixLength = length prefixComponents
            nameComponents = splitPath (name x)

toTreeEntry :: Index -> String -> (String, Maybe IndexEntry) -> Entry
toTreeEntry index prefix (name, Nothing) = Entry 0o040000 name (treeHash $ indexTreePrefix index (prefix ++ name ++ "/"))
toTreeEntry index prefix (name, Just ie) = Entry (mode ie) name (hash ie)

-- | Convert an index into a tree.
indexTree :: Index -> Tree
indexTree index = indexTreePrefix index ""
