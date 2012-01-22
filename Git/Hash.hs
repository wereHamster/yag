
module Git.Hash (

    -- The 'Hash' type
    Hash, hashData,

    -- Creating hashes
    nullHash, fromObject, fromString, fromBinaryByteString, fromHexByteString,

    -- Parsers
    binaryHash, stringHash,

    -- Miscellaneous
    abbrev

) where

import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AP (take)
import Data.Attoparsec.Char8 hiding (take)

import Data.ByteString.Base16 (encode, decode)

import Data.Digest.SHA1 (Word160(..), hash)

import Data.Bits
import Data.Char
import Data.Word


-- We hide the implementation of the hash behind this data. Internally the
-- hash is stored in a lazy bytestring. This makes it very easy to convert it
-- into other representations we need. At some later point, we might want to
-- store it in its original Word160 type. But that's far future.
data Hash = Hash { hashData :: S.ByteString } deriving (Eq)

-- | Showing a hash displays its contents in hex encoding.
instance Show Hash where
    show hash = map w2c $ S.unpack $ encode $ hashData hash



-- * Creating hashes

-- | The null hash is used quite often, for example to denote a non-existing
-- object. It is 20 bytes of zeros.
nullHash :: Hash
nullHash = Hash { hashData = S.pack $ take 20 $ repeat 0 }

-- | Build a Hash out of the object type and its data. This function will
-- generate the correct header, append the payload and compute the hash over
-- all of it.
fromObject :: String -> L.ByteString -> Hash
fromObject t d =
    unsafeCreate (hashByteString $ hash $ L.unpack $ L.append header d)
  where
    header = L.pack $ map (fromIntegral . ord) string
    string = t ++ " " ++ (show $ L.length d) ++ "\0"

-- | Build a hash from a strict 'ByteString' which contains the hash in binary.
fromBinaryByteString :: S.ByteString -> Hash
fromBinaryByteString = unsafeCreate

-- | Build a hash from a strict 'ByteString' which contains the hash in hex
-- encoded representation.
fromHexByteString :: S.ByteString -> Hash
fromHexByteString = unsafeCreate . fst . decode

-- | Build a hash from an ordinary String. The hash is assumed to be
-- hex-encoded, so this function is useful for example to parse user input.
fromString :: String -> Hash
fromString = unsafeCreate . fst . decode . S.pack . map c2w



-- * Parsers

-- | Binary and hex encoded hashes.
binaryHash, stringHash :: Parser Hash
binaryHash = fromBinaryByteString <$> AP.take 20
stringHash = fromHexByteString <$> AP.take 40


-- * Miscellaneous

-- | Return the first n characters of the string representation.
abbrev :: Int -> Hash -> String
abbrev n hash = take n $ show hash



-- * Internal stuff

-- Create a hash while checking that the input bytestring is exactly 20 bytes
-- (the size of a SHA1 in binary representation). If that's not the case then
-- raise an error. So use with care!
unsafeCreate :: S.ByteString -> Hash
unsafeCreate input
    | S.length input == 20 = Hash input
    | otherwise            = error "Git.Hash: input is not 20 bytes"

-- | Convert a Word160 into a ByteString.
hashByteString :: Word160 -> S.ByteString
hashByteString hash =
    S.pack $ concat $ map toBytes $ toWords hash
  where
    toWords (Word160 a b c d e) = a : b : c : d : e : []
    toBytes a = (w (s a 24)) : (w (s a 16)) : (w (s a 8)) : (w a) : []
        where w = fromIntegral; s = shiftR
