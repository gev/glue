module Reacthome.Auth.Domain.Credential.PublicKey where

import Control.Monad.Trans.Except
import Data.ByteString
import Data.Hashable
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ED25519 qualified as ED25519
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ES256 qualified as ES256
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.RS256 qualified as RS256
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.User.Id
import Util.ASN1
import Prelude hiding (head, length, splitAt)

data PublicKey = PublicKey
    { id :: PublicKeyId
    , userId :: UserId
    , algorithm :: PublicKeyAlgorithm
    , bytes :: ByteString
    }
    deriving stock (Show)

{-
    TODO: Refactor PublicKey. Use the handle pattern
-}

instance Eq PublicKey where
    a == b = a.id == b.id

instance Hashable PublicKey where
    hashWithSalt salt publicKey =
        hashWithSalt salt publicKey.id

decodePublicKey ::
    (Monad m) =>
    PublicKeyAlgorithm ->
    ByteString ->
    ExceptT String m ByteString
decodePublicKey algorithm bytes =
    derDecode bytes
        >>= case algorithm of
            ED25519 -> ED25519.decodePublicKey
            ES256 -> ES256.decodePublicKey
            RS256 -> RS256.decodePublicKey

verifySignature ::
    (Monad m) =>
    PublicKey ->
    ByteString ->
    ByteString ->
    ExceptT String m Bool
verifySignature key =
    case key.algorithm of
        ED25519 -> ED25519.verifySignature key.bytes
        ES256 -> ES256.verifySignature key.bytes
        RS256 -> RS256.verifySignature key.bytes
