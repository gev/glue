module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ED25519 where

import Control.Monad.Trans.Except
import Data.ASN1.BitArray
import Data.ASN1.Prim
import Data.ByteString

decodePublicKey :: (Monad m) => [ASN1] -> ExceptT String m ByteString
decodePublicKey = \case
    [ Start Sequence
        , Start Sequence
        , OID [1, 3, 101, 112] -- Ed25519 algorithm identifier
        , End Sequence
        , BitString (BitArray 256 bytes) -- 32 bytes for Ed25519 public key
        , End Sequence
        ] -> pure bytes
    _ -> throwE "Invalid Ed25519 public key format"
