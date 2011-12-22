
module Git.Repository where

import System.IO
import System.Directory

import Data.ByteString.Internal
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Codec.Compression.Zlib

import Data.Attoparsec.ByteString hiding (take)
import Data.Word

import Git.Hash
import Git.Object
import Git.Parser

data Repository = Repository {
    repositoryDataDirectory :: FilePath,
    repositoryWorkingDirectory :: FilePath
} deriving (Eq, Show)

objectStore :: Repository -> FilePath
objectStore repo = (repositoryDataDirectory repo) ++ "/objects"

objectPath :: Repository -> Hash -> FilePath
objectPath repo hash = (objectStore repo) ++ "/" ++ x ++ "/" ++ y where
    x = take 2 $ show hash
    y = drop 2 $ show hash

loadObject :: Repository -> Hash -> IO Object
loadObject repo hash = do
    handle <- openFile (objectPath repo hash) ReadMode
    hSetBinaryMode handle True
    contents <- L.hGetContents handle

    decompData <- return $ S.concat $ L.toChunks $ decompress contents
    object <- return $ parseOnly gitObject decompData
    case object of
        Right a -> return a
        otherwise -> error "couldn't parse object"

dumpObjectAtPath :: FilePath -> IO ()
dumpObjectAtPath path = do
    putStrLn path
    handle <- openFile ("test/" ++ path) ReadMode
    hSetBinaryMode handle True
    contents <- L.hGetContents handle

    decompData <- return $ S.concat $ L.toChunks $ decompress contents
    object <- return $ parseOnly gitObject decompData
    case object of
        Right a -> putStrLn (show a)
        otherwise -> error "couldn't parse object"

test :: IO ()
test = do
    dir <- getDirectoryContents "test"
    mapM dumpObjectAtPath $ drop 2 dir
    return ()
