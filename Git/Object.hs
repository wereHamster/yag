{-# LANGUAGE OverloadedStrings #-}  

module Git.Object (

    -- The 'Type' type
    Type(..), typeString, objectType,

    -- The 'Object' type
    Object(..), objectParser

) where

import Control.Applicative
import Data.Attoparsec.Char8 hiding (take)

import Git.Object.Blob
import Git.Object.Commit
import Git.Object.Tag
import Git.Object.Tree


-- * Type

-- | The four basic object types.
data Type = TBlob | TCommit | TTag | TTree


-- | Convert a type to its string representation.
typeString :: Type -> String
typeString TBlob   = "blob"
typeString TCommit = "commit"
typeString TTag    = "tag"
typeString TTree   = "tree"

-- | Parser for the object typ.
objectType :: Parser Type
objectType = blob <|> commit <|> tag <|> tree where
    blob   = TBlob   <$ string "blob"
    commit = TCommit <$ string "commit"
    tag    = TTag    <$ string "tag"
    tree   = TTree   <$ string "tree"


-- * Object

-- | The actual objects.
data Object = Blob Blob | Commit Commit | Tag Tag | Tree Tree
    deriving (Show)


-- | Parser for the 'Object' type.
objectParser :: Parser Object
objectParser =
    header >>= objectBuilder
  where
    header = (,) <$> (objectType <* space) <*> (decimal <* (char '\0'))

    objectBuilder (TBlob,   length) = Blob   <$> blobParser length
    objectBuilder (TCommit, length) = Commit <$> commitParser
    objectBuilder (TTag,    length) = Tag    <$> tagParser
    objectBuilder (TTree,   length) = Tree   <$> treeParser
