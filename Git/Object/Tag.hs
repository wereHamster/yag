
module Git.Object.Tag (

    -- The 'Tag' type
    Tag,

    tagObject, tagObjectType, tagTagger, tagTaggerDate, tagName, tagMessage,

    emptyTag, tagParser

) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Data.Attoparsec.Char8 hiding (take)

import System.Locale
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Data.Char

import Git.Hash
import Git.Parser
import Git.Identity

data Tag = Tag {
    -- The object, in terms of hash and type.
    tagObject :: Hash, tagObjectType :: String,

    -- The identify of the person who created the tag.
    tagTagger :: Identity, tagTaggerDate :: ZonedTime,

    -- A name and message.
    tagName :: String, tagMessage :: String
}

emptyTag :: Tag
emptyTag = Tag nullHash "" defaultIdentity time "" "" where
    time = utcToZonedTime utc $ posixSecondsToUTCTime $ realToFrac 0

instance Show Tag where
    show tag = (unlines $ concat $ headers tag) ++ "\n" ++ (tagMessage tag) where
        headers tag = map ($tag) [ object, typ, name, tagger ]
        timestamp   = formatTime defaultTimeLocale "%s %z"

        object  tag = [ "object " ++ (show $ tagObject tag) ]
        typ     tag = [ "type " ++ (tagObjectType tag) ]
        name    tag = [ "tag " ++ (tagName tag) ]
        tagger  tag = [ "tagger " ++ (show $ tagTagger tag) ++ " " ++ (timestamp $ tagTaggerDate tag) ]


tagHash :: Tag -> Hash
tagHash tag = fromObject "tag" tagData where
    tagData = L.pack $ map (fromIntegral . ord) $ show tag

-- * Parser
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
            tag { tagObjectType = toString value }
        | key == "tagger" =
            applyTagTagger tag value
        | otherwise = tag
