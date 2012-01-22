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
