module Reacthome.Auth.Domain.Credential.PublicKey.Id where

import Data.ByteString
import Data.Hashable

newtype PublicKeyId = PublicKeyId {value :: ByteString}
    deriving stock (Show)
    deriving newtype (Eq, Hashable)

mkPublicKeyId :: ByteString -> PublicKeyId
mkPublicKeyId = PublicKeyId
