
module Git.Hash (

    -- The 'Hash' type
    Hash,

    -- Creating hashes
    nullHash, fromObject, fromString, fromBinaryByteString, fromHexByteString,

    -- Miscellaneous
    abbrev

) where

import Data.ByteString.Internal
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Digest.SHA1 (Word160(..), hash)

import Data.Bits
import Data.Char
import Data.Word

import Git.Object


-- We hide the implementation of the hash behind this data. Internally the
-- hash is stored in a lazy bytestring. This makes it very easy to convert it
-- into other representations we need. At some later point, we might want to
-- store it in its original Word160 type. But that's far future.
data Hash = Hash { hashData :: L.ByteString } deriving (Eq)

hashFromHex :: L.ByteString -> Hash
hashFromHex hex = Hash $ L.pack $ decodeHex $ L.unpack hex


-- * Creating hashes

-- The null hash is used quite often, for example to denote a non-existing
-- object.
nullHash :: Hash
nullHash = Hash { hashData = L.pack $ take 20 $ repeat 0 }

-- Build a Hash out of the object type and its data.
fromObject :: Git.Object.Type -> L.ByteString -> Hash
fromObject t d =
    Hash (hashByteString $ hash $ L.unpack $ L.append header d)
  where
    header = L.pack $ map (fromIntegral . ord) string
    string = (typeString t) ++ " " ++ (show $ L.length d) ++ "\0"

-- Build a hash from a strict bytestring, which contains either the binary or
-- hex-encoded hash. This is most useful when reading the data from a file,
-- network or another stream.
fromBinaryByteString, fromHexByteString :: S.ByteString -> Hash
fromBinaryByteString a = Hash $ L.fromChunks [a]
fromHexByteString a    = hashFromHex $ L.fromChunks [a]

-- Build a hash from an ordinary String. The hash is assumed to be
-- hex-encoded, so this function is useful for example to parse user input.
fromString :: String -> Hash
fromString hex = hashFromHex $ L.pack $ map c2w hex


-- * Internal stuff

hashByteString :: Word160 -> L.ByteString
hashByteString hash =
    L.pack $ concat $ map toBytes $ toWords hash
  where
    toWords (Word160 a b c d e) = a : b : c : d : e : []
    toBytes a = (w (s a 24)) : (w (s a 16)) : (w (s a 8)) : (w a) : []
        where w = fromIntegral; s = shiftR


-- The hex encoding and decoding functions operate on [Word8]. Apparently you
-- can't foldr over ByteString, or something like that. In any case, ghc
-- didn't like what I had.
encodeHex :: [Word8] -> [Word8]
encodeHex =
    foldr paddedShowHex []
  where
    paddedShowHex x xs = digit (x `shiftR` 4) : digit (x .&. 0xf) : xs
    digit x = c2w $ intToDigit $ fromIntegral x

decodeHex :: [Word8] -> [Word8]
decodeHex [] = []
decodeHex (x:y:r) =
    ((c x) * 16 + (c y)) : decodeHex r
  where
    c x
        | x >= 48 && x <=  57 = x - 48
        | x >= 65 && x <=  70 = x - 65 + 10
        | x >= 97 && x <= 102 = x - 97 + 10


-- Showing a hash displays its contents in hex encoding.
instance Show Hash where
    show hash = map w2c $ encodeHex $ L.unpack $ hashData hash

abbrev :: Int -> Hash -> String
abbrev n hash = take n $ show hash
