
module Git.Repository (

    -- The 'Repository' type
    Repository,

    -- Locating repositories
    findRepository, gitDir, loadObject, commitParent, walkAncestors,
    peelTo

) where

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
import Git.Object.Blob
import Git.Object.Commit (commitHash, commitParents, commitTree)
import Git.Object.Tag
import Git.Object.Tree
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
    object <- return $ parseOnly objectParser decompData
    case object of
        Right a -> return a
        otherwise -> error "couldn't parse object"

peelTo :: Type -> Object -> IO (Maybe Hash)

-- Commits can be peeled to itself or the tree.
peelTo TCommit (Commit o) = return $ Just $ commitHash o
peelTo TTree   (Commit o) = return $ Just $ commitTree o

commitParent :: Int -> Object -> IO (Maybe Hash)
commitParent n (Commit c)
    | n < 0 || n > (length $ commitParents c) = return Nothing
    | otherwise = return $ Just $ commitParents c !! (n - 1)
commitParent _ _ = return Nothing

walkAncestors :: Repository -> Int -> Object -> IO (Maybe Hash)
walkAncestors repo n (Commit c)
    | n == 0 = return $ Just $ commitHash c
    | null $ commitParents c = return Nothing
    | otherwise = loadObject repo (head $ commitParents c) >>= walkAncestors repo (n - 1)
walkAncestors _ _ _ = return Nothing

toCommit :: Object -> IO Object
toCommit obj@(Commit commit) = return obj
toCommit _ = error "Not a commit"

dumpObjectAtPath :: FilePath -> IO ()
dumpObjectAtPath path = do
    putStrLn path
    handle <- openFile ("test/" ++ path) ReadMode
    hSetBinaryMode handle True
    contents <- L.hGetContents handle

    decompData <- return $ S.concat $ L.toChunks $ decompress contents
    object <- return $ parseOnly objectParser decompData
    case object of
        Right a -> putStrLn (show a)
        otherwise -> error "couldn't parse object"

test :: IO ()
test = do
    dir <- getDirectoryContents "test"
    mapM dumpObjectAtPath $ drop 2 dir
    return ()
