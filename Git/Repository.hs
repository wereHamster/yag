
module Git.Repository where

import System.IO
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as L
import Codec.Compression.Zlib

import Data.Word

import Git.Hash
import Git.Object

data Repository = Repository {
    repositoryDataDirectory :: FilePath,
    repositoryWorkingDirectory :: FilePath
} deriving (Eq, Show)

objectStore :: Repository -> FilePath
objectStore repo = (repositoryDataDirectory repo) ++ "/objects"

objectPath :: Repository -> Hash -> FilePath
objectPath repo hash = (objectStore repo) ++ "/" ++ x ++ "/" ++ y
    where
        x = take 2 $ show hash
        y = drop 2 $ show hash

loadObject :: Repository -> Hash -> IO String
loadObject repo hash = do
    handle <- openFile (objectPath repo hash) ReadMode
    hSetBinaryMode handle True
    contents <- L.hGetContents handle

    return $ map Data.ByteString.Internal.w2c $ L.unpack $ decompress contents
