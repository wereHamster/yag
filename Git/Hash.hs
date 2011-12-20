
module Git.Hash where

import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as L

import Data.Digest.SHA1

import Data.Bits
import Data.Char
import Data.Word

import Git.Object


-- We hide the implementation of the hash behind this data. Internally the
-- hash is stored in a lazy bytestring. This makes it very easy to convert it
-- into other representations we need. At some later point, we might want to
-- store it in its original Word160 type. But that's far future.
data Hash = Hash { hashData :: L.ByteString } deriving (Eq)


-- We support conversion between Hash and ByteString. Because most of our
-- input we get in the form of ByteString (file etc).
hashFromHex :: L.ByteString -> Hash
hashFromHex hex = Hash $ L.pack $ decodeHex $ L.unpack hex

hashFromHexString :: String -> Hash
hashFromHexString hex = hashFromHex $ L.pack $ map c2w hex


hashByteString :: Word160 -> L.ByteString
hashByteString hash = L.pack $ concat $ map toBytes $ toWords hash
    where
        toWords (Word160 a b c d e) = a : b : c : d : e : []
        toBytes a = (w (s a 24)) : (w (s a 16)) : (w (s a 8)) : (w a) : []
            where w = fromIntegral; s = shiftR


-- The hex encoding and decoding functions operate on [Word8]. Apparently you
-- can't foldr over ByteString, or something like that. In any case, ghc
-- didn't like what I had.
encodeHex :: [Word8] -> [Word8]
encodeHex = foldr paddedShowHex []
        where
            paddedShowHex x xs = digit (x `shiftR` 4) : digit (x .&. 0xf) : xs
            digit x = c2w $ intToDigit $ fromIntegral x

decodeHex :: [Word8] -> [Word8]
decodeHex [] = []
decodeHex (x:y:r) = ((c x) * 16 + (c y)) : decodeHex r
    where
        c x
            | x >= 48 && x <=  57 = x - 48
            | x >= 65 && x <=  70 = x - 65 + 10
            | x >= 97 && x <= 102 = x - 97 + 10


-- Helper function to hash an object. Give the type and its contents (without
-- the header!), and we'll return the Hash.
hashFromObject :: Git.Object.Type -> L.ByteString -> Hash
hashFromObject t d = Hash (hashByteString $ hash $ L.unpack $ L.append header d)
    where
        header = L.pack $ map (fromIntegral . ord) string
        string = (typeString t) ++ " " ++ (show $ L.length d) ++ "\0"

-- This is just for convenience.
nullHash :: Hash
nullHash = Hash { hashData = L.pack $ take 20 $ repeat 0 }


-- Showing a hash displays its contents in hex encoding.
instance Show Hash where
    show hash = map w2c $ encodeHex $ L.unpack $ hashData hash
