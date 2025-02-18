module Reacthome.Auth.Domain.Credential.PublicKey where

import Data.ByteString
import Data.Hashable
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.User.Id

data PublicKey = PublicKey
    { id :: PublicKeyId
    , userId :: UserId
    , algorithm :: PublicKeyAlgorithm
    , bytes :: ByteString
    }
    deriving stock (Show)

instance Eq PublicKey where
    a == b = a.id == b.id

instance Hashable PublicKey where
    hashWithSalt salt publicKey =
        hashWithSalt salt publicKey.id

-- decodePublicKey :: PublicKey -> ExceptT String m PublicKey
-- decodePublicKey publicKey = case publicKey.algorithm of
--     ED25519 -> pure decodePublicKeyES256 publicKey.bytes
--     ES256 -> pure publicKey
--     RS256 -> pure publicKey

-- decodePublicKeyES256 :: ByteString -> ExceptT String m PublicKey
