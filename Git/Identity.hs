
module Git.Identity where

import Data.DateTime
import System.IO

-- Identity is a name + email address.
data Identity = Identity { identityName :: String, identityEmail :: String } deriving (Eq)

defaultIdentity :: Identity
defaultIdentity = Identity "Tomas Carnecky" "tomas.carnecky@gmail.com"

loadDefaultIdentity :: IO Identity
loadDefaultIdentity = return defaultIdentity

instance Show Identity where
    show identity = (identityName identity) ++ " <" ++ (identityEmail identity) ++ ">"
