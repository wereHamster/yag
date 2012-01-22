
module Git.Ref (

    -- The 'Ref' type
    Ref,

    -- Stuff to do with a ref
    fullNameRef, resolveRef

) where

import Data.Char
import Data.List
import System.FilePath
import System.Directory

import Git.Hash
import Git.Repository

-- Refs are either symbolic or direct hash references.
data Ref = Symref String | Direct String


-- | Expand a string into a full ref, according to the rules set by git.
-- For example: "master"" -> "refs/heads/master"
fullNameRef :: Repository -> String -> IO (Maybe String)
fullNameRef repo name = firstTrue $ map testPath sympaths where

    testPath path = do
      found <- testRef path
      if found
        then return $ Just path
        else return Nothing

    testRef :: String -> IO Bool
    testRef path = do
      found <- doesFileExist ((gitDir repo) </> path)
      if found
        then return True
        else return False

    -- List of paths to search from "git help rev-parse".
    prefixes = ["", "refs", "refs/tags", "refs/heads", "refs/remotes"]
    sympaths = map (</> name) prefixes ++ ["refs/remotes" </> name </> "HEAD"]

-- First expand the string then recursively resolve the ref into a hash.
resolveRef :: Repository -> String -> IO (Maybe Hash)
resolveRef repo ref = expand repo ref >>= resolve repo



-- * Internal

-- This is a pretty basic parser of the lose ref files.
parse :: String -> Maybe Ref
parse s
    | take 4 s == "ref:" = Just $ Symref (stripWhitespace $ drop 4 s)
    | length s >= 40     = Just $ Direct (stripWhitespace s)
    | otherwise          = Nothing

-- Read the file and parse it into a Ref. The path must be fully expanded.
readRef :: Repository -> String -> IO (Maybe Ref)
readRef repo path = readFile ((gitDir repo) </> path) >>= return . parse

-- Expands a name and returns it as symref.
expand :: Repository -> String -> IO (Maybe Ref)
expand repo ref = fullNameRef repo ref >>= asSymref where
    asSymref Nothing  = return $ Nothing
    asSymref (Just a) = readRef repo a

-- This is recursive, handles symrefs and such.
resolve :: Repository -> Maybe Ref -> IO (Maybe Hash)
resolve _     Nothing             = return Nothing
resolve repo (Just (Symref path)) = readRef repo path >>= resolve repo
resolve repo (Just (Direct hash)) = return $ Just $ fromString hash

firstTrue :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstTrue []     = return Nothing
firstTrue (x:xs) = do
    test <- x
    case test of
        Just _ -> return test
        Nothing -> firstTrue xs

stripWhitespace :: String -> String
stripWhitespace = reverse . dropSpace . reverse . dropSpace where
    dropSpace = dropWhile isSpace
