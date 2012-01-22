
module Git.Object.Tree (

    -- The 'Tree' type
    Tree(treeEntries), Entry(..),

    emptyTree, emptyTreeHash, treeParser, treeHash

) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Applicative
import Data.Attoparsec.Char8 hiding (take)

import Numeric
import Data.Bits
import Data.List
import Data.Char
import Text.Printf

import Git.Hash
import Git.Parser

data Entry = Entry {
    entryMode :: Int, entryPath :: String, entryHash :: Hash
} deriving (Eq)

instance Show Entry where
    show entry = concat $ intersperse " " [ mode, typ, hash, path ] where
        mode = formatMode $ entryMode entry
        typ  = if ((entryMode entry) .&. 0o040000) == 0o040000 then "tree" else "blob"
        hash = show $ entryHash entry
        path = entryPath entry

        formatMode mode = printf "%06o" mode :: String

-- | A tree is basically a list of entries.
data Tree = Tree { treeEntries :: [Entry] } deriving (Eq)

-- | An empty tree is one with zero entries.
emptyTree :: Tree
emptyTree = Tree []

-- The magic empty tree hash. It's not hardcoded anywhere, instead we generate
-- it as needed. But we should still check somewhere that we're generating the
-- correct hash: 4b825dc642cb6eb9a060e54bf8d69288fbee4904
emptyTreeHash :: Hash
emptyTreeHash = treeHash $ emptyTree

instance Show Tree where
    show tree = unlines $ map show $ treeEntries tree

-- TODO: Clean this up, use a Builder instead of, well, the other crap we use
-- now.
treeHash :: Tree -> Hash
treeHash tree = fromObject "tree" treeData where
    treeData = L.concat $ map buildEntry (treeEntries tree)

buildEntry :: Entry -> L.ByteString
buildEntry entry = L.concat [part1, L.fromChunks [d]] where
    d = hashData $ entryHash $ entry
    part1 = L.pack $ map c2w $ mode ++ " " ++ (entryPath entry) ++ "\0" where
        mode = (showOct $ entryMode entry) ""

-- * Parser
treeParser :: Parser Tree
treeParser = Tree <$> many1 treeEntry where
    treeEntry = Entry <$> (octal <* space) <*> nullString <*> binaryHash
