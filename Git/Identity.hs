
module Git.Identity (

    -- The 'Identity' type
    Identity(..),

    -- Creating identities
    defaultIdentity,

    -- Parser
    identity, identTime

) where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AP (takeWhile)
import Data.Attoparsec.Char8 hiding (take)

import System.Locale
import Data.Time

import Git.Utils
import Git.Parser


-- | Identity is a name + email address.
data Identity = Identity { identityName :: String, identityEmail :: String } deriving (Eq)

-- | Showing the identity will convert it to the standard format used in Git,
-- somewhat resembling what is used in emails: A U Thor <a.u.thor@example.com>
instance Show Identity where
    show identity = (identityName identity) ++ " <" ++ (identityEmail identity) ++ ">"


-- | The default identity is the author of this tool. Because he is selfish.
-- He also hopes nobody will notice and thus create commits in his name. It's
-- part of his plan for world domination.
defaultIdentity :: Identity
defaultIdentity = Identity "Tomas Carnecky" "tomas.carnecky@gmail.com"


-- * Parser

-- | The identity is "name <email>".
identity :: Parser Identity
identity = ctor <$> name <*> email where
    ctor name email = Identity (strip $ toString name) (strip $ toString email)
    name  = AP.takeWhile (not . (== c2w '<')) <* char '<'
    email = AP.takeWhile (not . (== c2w '>')) <* char '>'

-- | Identity + ZonedTime, space separated, as it appears in header lines.
identTime :: Parser (Identity, ZonedTime)
identTime = (,) <$> identity <* space <*> timestamp
