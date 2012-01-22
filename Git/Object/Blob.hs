
module Git.Object.Blob (

    -- The 'Blob' type
    Blob,

    blobParser

) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AP (Parser, take)

import Git.Hash
import Git.Parser


-- | A Blob is just a blob of data
data Blob = Blob { blobData :: L.ByteString } deriving (Eq, Show)


-- * Hashing
blobHash :: Blob -> Hash
blobHash blob = fromObject "blob" (blobData blob)


-- * Parser

-- | We don't have to parse anything, just take what we can get and wrap it
-- with the Blob constructor. 
blobParser :: Int -> AP.Parser Blob
blobParser len = Blob . L.fromChunks . (:[]) <$> AP.take len
