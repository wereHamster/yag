
module Git.Revision where

import           Control.Monad (foldM)

import qualified Data.ByteString as S
import           Data.List
import           Data.Maybe
import           Data.Time


import Control.Applicative
import qualified Data.Attoparsec.ByteString as AP (word8, inClass, take, takeWhile, takeTill)
import Data.Attoparsec.Char8 hiding (take)

import qualified Git.Hash as H
import Git.Object.Commit
import Git.Object
import Git.Parser
import Git.Repository
import Git.Ref

-- We start with a base, and modify it until we get the revision we want. This
-- approach makes it quite easy to write the parser, as we parse the rev from
-- left to right, starting with a base followed by many modifiers.
data Base = Hash H.Hash | Describe String | Refname String | Index | Any
instance Show Base where
    show (Refname a)    = a


data Modifier = Parent Int | Ancestor Int | Peel Git.Object.Type | Reflog Int |
    Date UTCTime | Branch Int | Upstream | Nontag | Regex String | Path String
instance Show Modifier where
    show (Parent a)     = "^" ++ (show a)
    show (Ancestor a)   = "~" ++ (show a)
    show (Peel a)       = "^{" ++ (typeString a) ++ "}"
    show (Reflog a)     = "@{" ++ (show a) ++ "}"
    show (Nontag)       = "^{}"
    show (Path a)       = ":" ++ a


data Revision = Revision { revisionBase :: Base, revisionModifiers :: [Modifier] }
instance Show Revision where
    show rev = (show $ revisionBase rev) ++ (concat $ map show $ revisionModifiers rev)


-- The parsers for the base
hash, describe, refname, index, any :: Parser Base
hash     = Hash                <$> stringHash
describe = Describe . toString <$> AP.takeWhile (AP.inClass "a-z")
refname  = Refname  . toString <$> AP.takeWhile (AP.inClass "a-zA-Z0-9./_-")
index    = Index               <$  char ':'
any      = Any                 <$  char ':'

base :: Parser Base
base = hash <|> refname <|> describe <|> index <|> Git.Revision.any


-- The parsers for the modifiers
parent, ancestor, peel, reflog, date, branch, upstream, nontag, regex, path :: Parser Modifier
parent = ctor <$  char '^' <*> optional decimal where
    ctor (Just a) = Parent a
    ctor Nothing  = Parent 1
ancestor = ctor <$  char '~' <*> optional decimal where
    ctor (Just a) = Ancestor a
    ctor Nothing  = Ancestor 1
peel = Peel <$ char '^' <* char '{' <*> objectType <* char '}'
reflog = Reflog <$ char '@' <* char '{' <*> decimal <* char '}'
nontag = Nontag <$ char '^' <* char '{' <* char '}'
path = Path <$ char ':' <*> remString

date = peel
branch = peel
upstream = peel
regex = peel

modifier :: Parser Modifier
modifier = nontag <|> peel <|> parent <|> ancestor <|> reflog <|> path

-- The parser for an actual revision.
revision :: Parser Revision
revision = Revision <$> base <*> many modifier <* endOfInput

parseRevision :: String -> Maybe Revision
parseRevision input = case parseOnly revision (S.pack $ Prelude.map c2w input) of
    Left _  -> Nothing
    Right a -> Just a

resolveBase :: Repository -> Base -> IO (Maybe H.Hash)
resolveBase repo base = do
    case base of
        Hash a -> return $ Just a
        Refname a -> do
            x <- fullNameRef repo a
            resolveRef repo $ fromJust x
        otherwise -> return Nothing

applyModifiers :: Repository -> [Modifier] -> Maybe H.Hash -> IO (Maybe H.Hash)
applyModifiers repo modifiers base0 =
    foldM apply base0 modifiers
  where
    -- SM: No monad required. Just some twiddling with argument structure.
    -- I think now it is also easier to understand what's happening.
    apply Nothing     _        = return Nothing
    apply (Just base) modifier = do
        obj <- loadObject repo base 
        case modifier of
          Parent n   -> commitParent n obj
          Ancestor n -> walkAncestors repo n obj
          Peel t     -> peelTo t obj

-- Turn a Revision into a Hash (if possible).
resolveRevision :: Repository -> Revision -> IO (Maybe H.Hash)
resolveRevision repo rev = resolveBase repo base >>= applyModifiers repo modifiers where
    base = revisionBase rev; modifiers = revisionModifiers rev

testData = S.pack $ Prelude.map c2w "foo^2^~^1^{commit}@{2}"
test = case parseOnly revision testData of
    Left err -> error $ show err
    Right r -> r
