
module Git.Object.Blob (

    -- The 'Blob' type
    Blob(..)

) where

import qualified Data.ByteString.Lazy as L

import Git.Hash
import Git.Object


-- | A Blob is just a blob of data
data Blob = Blob { blobData :: L.ByteString } deriving (Eq, Show)

blobHash :: Blob -> Hash
blobHash blob = fromObject Git.Object.Blob (blobData blob)
