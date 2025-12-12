module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ED25519 where

import Data.ASN1.BitArray
import Data.ASN1.Prim
import Data.ByteString

decodePublicKey :: [ASN1] -> Either String ByteString
decodePublicKey = \case
    [ Start Sequence
        , Start Sequence
        , OID [1, 3, 101, 112] -- Ed25519 algorithm identifier
        , End Sequence
        , BitString (BitArray 256 bytes) -- 32 bytes for Ed25519 public key
        , End Sequence
        ] -> Right bytes
    _ -> Left "Invalid Ed25519 public key format"

verifySignature ::
    ByteString ->
    ByteString ->
    ByteString ->
    Either String Bool
verifySignature _ _ _ = Left "ED25519 verify signature not implemented"

{-
    TODO: WebAuthn. Implement ED25519
-}
