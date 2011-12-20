
module Git.Object where

import Data.Char

-- The four basic object types.
data Type = Blob | Tree | Commit | Tag deriving (Eq, Show)

typeString :: Type -> String
typeString = map toLower . show


