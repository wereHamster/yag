
module Git.Object.Tree where

import qualified Data.ByteString.Lazy as L
import Numeric
import Data.List
import Data.Char
import Text.Printf

import qualified Git.Object as Object
import Git.Hash

data Entry = Entry {
    entryMode :: Int, entryType :: Object.Type, entryHash :: Hash,
    entryName :: String
} deriving (Eq)

instance Show Entry where
    show entry = concat $ intersperse " " $ map ($entry) [ mode, otyp, hash, name ]
        where
            mode entry = formatMode $ entryMode entry
            otyp entry = Object.typeString $ entryType entry
            hash entry = show $ entryHash entry
            name entry = entryName entry

            formatMode mode = printf "%06o" mode :: String

data Tree = Tree { treeEntries :: [Entry] } deriving (Eq)

-- The magic empty tree hash. It's not hardcoded anywhere, instead we generate
-- it as needed.
emptyTreeHash :: Hash
emptyTreeHash = treeHash $ Tree []

instance Show Tree where
    show tree = unlines $ map show $ treeEntries tree

treeHash :: Tree -> Hash
treeHash tree = hashFromObject Object.Tree treeData
    where
        treeData = L.pack $ map (fromIntegral . ord) $ show tree
