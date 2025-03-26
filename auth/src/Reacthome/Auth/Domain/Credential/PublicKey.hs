module Reacthome.Auth.Domain.Credential.PublicKey where

import Control.Monad.Trans.Except
import Data.ASN1.Prim
import Data.ByteString
import Data.Hashable
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ED25519 qualified as ED25519
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ES256 qualified as ES256
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.RS256 qualified as RS256
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.User.Id
import Prelude hiding (head, length, splitAt)

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

decodePublicKey ::
    (Monad m) =>
    PublicKeyAlgorithm ->
    [ASN1] ->
    ExceptT String m ByteString
decodePublicKey = \case
    ED25519 -> ED25519.decodePublicKey
    ES256 -> ES256.decodePublicKey
    RS256 -> RS256.decodePublicKey
