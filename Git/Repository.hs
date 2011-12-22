
module Git.Repository where

import Control.Exception

import System.IO
import System.FilePath
import System.Directory

import Data.ByteString.Internal
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Codec.Compression.Zlib

import Data.Attoparsec.ByteString hiding (take)
import Data.Word

import Git.Hash
import Git.Object
import Git.Object.Commit
import Git.Parser

data Repository = Repository {
    gitDir :: FilePath, workingDir:: FilePath
} deriving (Eq, Show)

thisRepo :: Repository
thisRepo = Repository ".git" ""

-- Starting at a path, find the repository root (directory with a .git dir).
findRepository :: FilePath -> IO (Maybe Repository)
findRepository path = do
    let gitDir = path </> ".git"
    exist <- doesDirectoryExist gitDir
    if exist
        then return $ Just $ Repository gitDir path
        else if path == "/"
            then return Nothing
            else findRepository (takeDirectory path)


objectStore :: Repository -> FilePath
objectStore repo = (gitDir repo) ++ "/objects"

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

toCommit :: Object -> IO Commit
toCommit (GitCommit commit) = return commit
toCommit _ = error "Not a commit"

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
