
module Git.Identity (

    -- The 'Identity' type
    Identity(..),

    -- Creating identities
    defaultIdentity

) where


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
