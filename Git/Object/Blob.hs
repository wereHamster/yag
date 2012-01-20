
module Git.Object.Blob where

import qualified Data.ByteString.Lazy as L

import Git.Hash
import Git.Object

data Blob = Blob { blobData :: L.ByteString } deriving (Eq, Show)

blobHash :: Blob -> Hash
blobHash blob = fromObject Git.Object.Blob (blobData blob)
