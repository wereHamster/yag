
module Git.Object where

import Data.ByteString.Internal
import qualified Data.ByteString as S

import Control.Applicative
import qualified Data.Attoparsec as AP (takeWhile)
import Data.Attoparsec.Char8 hiding (take)

import Data.Char

-- The four basic object types. Plus an invalid type.
data Type = Invalid | Blob | Tree | Commit | Tag deriving (Eq, Show)

typeString :: Type -> String
typeString = map toLower . show

typeFromString :: String -> Type
typeFromString "blob"   = Blob
typeFromString "commit" = Commit
typeFromString "tag"    = Tag
typeFromString "tree"   = Tree
typeFromString _        = Invalid
