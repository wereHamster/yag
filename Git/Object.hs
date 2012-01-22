{-# LANGUAGE OverloadedStrings #-}  

module Git.Object (

    -- The 'Object' type
    Object(..), Type(..),

    typeString, objectParser, objectType

) where

import Data.Char (toLower)

import Control.Applicative
import Data.Attoparsec.Char8 hiding (take)

import Git.Object.Blob
import Git.Object.Commit
import Git.Object.Tag
import Git.Object.Tree

-- | The four basic object types.
data Type = TBlob | TCommit | TTag | TTree

typeString :: Type -> String
typeString TBlob = "blob"

data Object = Blob Blob | Commit Commit | Tag Tag | Tree Tree
    deriving (Show)


-- * Parser
objectParser :: Parser Object
objectParser = header >>= objectBuilder where
    header = (,) <$> (objectType <* space) <*> (decimal <* (char '\0'))

objectType :: Parser Type
objectType = blob <|> commit <|> tag <|> tree where
    blob   = TBlob   <$ string "blob"
    commit = TCommit <$ string "commit"
    tag    = TTag    <$ string "tag"
    tree   = TTree   <$ string "tree"

-- Return the proper parser for the given object type.
objectBuilder :: (Type, Int) -> Parser Object
objectBuilder (TBlob,   length) = Blob   <$> blobParser length
objectBuilder (TCommit, length) = Commit <$> commitParser
objectBuilder (TTag,    length) = Tag    <$> tagParser
objectBuilder (TTree,   length) = Tree   <$> treeParser
