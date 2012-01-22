
module Git.Object (

    -- The 'Type' type
    Type(..),

    typeString, typeFromString

) where

import Data.Char (toLower)


-- | The four basic object types. Plus an invalid type.
data Type = Invalid | Blob | Tree | Commit | Tag deriving (Eq, Show)


-- | Return the type string for the given type.
typeString :: Type -> String
typeString = map toLower . show

-- | Convert a type string into the actual type.
typeFromString :: String -> Type
typeFromString "blob"   = Blob
typeFromString "commit" = Commit
typeFromString "tag"    = Tag
typeFromString "tree"   = Tree
typeFromString _        = Invalid
