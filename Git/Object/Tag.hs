
module Git.Object.Tag where

import qualified Data.ByteString.Lazy as L

import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Data.Char

import Git.Object
import Git.Hash
import Git.Identity

data Tag = Tag {
    -- The object, in terms of hash and type.
    tagObject :: Hash, tagObjectType :: Git.Object.Type,

    -- The identify of the person who created the tag.
    tagTagger :: Identity, tagTaggerDate :: ZonedTime,

    -- A name and message.
    tagName :: String, tagMessage :: String
}

emptyTag :: Tag
emptyTag = Git.Object.Tag.Tag nullHash Git.Object.Invalid defaultIdentity time "" "" where
    time = utcToZonedTime utc $ posixSecondsToUTCTime $ realToFrac 0

instance Show Tag where
    show tag = (unlines $ concat $ headers tag) ++ "\n" ++ (tagMessage tag) where
        headers tag = map ($tag) [ object, typ, name, tagger ]
        timestamp   = formatTime defaultTimeLocale "%s %z"

        object  tag = [ "object " ++ (show $ tagObject tag) ]
        typ     tag = [ "type " ++ (typeString $ tagObjectType tag) ]
        name    tag = [ "tag " ++ (tagName tag) ]
        tagger  tag = [ "tagger " ++ (show $ tagTagger tag) ++ " " ++ (timestamp $ tagTaggerDate tag) ]


tagHash :: Tag -> Hash
tagHash tag = fromObject Git.Object.Tag tagData where
    tagData = L.pack $ map (fromIntegral . ord) $ show tag
