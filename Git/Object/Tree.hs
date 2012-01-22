
module Git.Object.Tree (

    -- The 'Tree' type
    Tree, Entry(..),

    emptyTreeHash, treeParser

) where

import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Data.Attoparsec.Char8 hiding (take)

import Numeric
import Data.List
import Data.Char
import Text.Printf

import Git.Hash
import Git.Parser

data Entry = Entry {
    entryMode :: Int, entryPath :: String, entryHash :: Hash
} deriving (Eq)

instance Show Entry where
    show entry = concat $ intersperse " " [ mode, hash, path ] where
        mode = formatMode $ entryMode entry
        hash = show $ entryHash entry
        path = entryPath entry

        formatMode mode = printf "%06o" mode :: String

-- | A tree is basically a list of entries.
data Tree = Tree { treeEntries :: [Entry] } deriving (Eq)

-- The magic empty tree hash. It's not hardcoded anywhere, instead we generate
-- it as needed. But we should still check somewhere that we're generating the
-- correct hash: 4b825dc642cb6eb9a060e54bf8d69288fbee4904
emptyTreeHash :: Hash
emptyTreeHash = treeHash $ Tree []

instance Show Tree where
    show tree = unlines $ map show $ treeEntries tree

treeHash :: Tree -> Hash
treeHash tree = fromObject "tree" treeData
    where
        treeData = L.pack $ map (fromIntegral . ord) $ show tree

-- * Parser
treeParser :: Parser Tree
treeParser = Tree <$> many1 treeEntry where
    treeEntry = Entry <$> (octal <* space) <*> nullString <*> binaryHash

