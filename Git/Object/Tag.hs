
module Git.Object.Tag where

import qualified Data.ByteString.Lazy as L

import System.Locale
import Data.Time
import Data.Char

import Git.Object
import Git.Hash
import Git.Identity

data Tag = Tag {
    tagObject :: Hash, tagType :: Git.Object.Type,
    tagTagger :: Identity, tagTaggerDate :: UTCTime,
    tagName :: String, tagMessage :: String
} deriving (Eq)

instance Show Tag where
    show tag = (unlines $ concat $ headers tag) ++ "\n" ++ (tagMessage tag)
        where
            headers tag = map ($tag) [ object, otype, name, tagger ]
            timestamp   = formatTime defaultTimeLocale "%s %z"

            object  tag = [ "object " ++ (show $ tagObject tag) ]
            otype   tag = [ "type " ++ (typeString $ tagType tag) ]
            name    tag = [ "tag " ++ (tagName tag) ]
            tagger  tag = [ "tagger " ++ (show $ tagTagger tag) ++ " " ++ (timestamp $ tagTaggerDate tag) ]


tagHash :: Tag -> Hash
tagHash tag = hashFromObject Git.Object.Tag tagData
    where
        tagData = L.pack $ map (fromIntegral . ord) $ show tag
