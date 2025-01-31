module Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier where

import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm

type COSEAlgorithmIdentifier = Int

pattern ED25519' :: COSEAlgorithmIdentifier
pattern ED25519' = -8

pattern ES256' :: COSEAlgorithmIdentifier
pattern ES256' = -7

pattern RS256' :: COSEAlgorithmIdentifier
pattern RS256' = -257

decodePublicKeyAlgorithm ::
    COSEAlgorithmIdentifier ->
    Either String PublicKeyAlgorithm
decodePublicKeyAlgorithm = \case
    ED25519' -> Right ED25519
    ES256' -> Right ES256
    RS256' -> Right RS256
    _ -> Left "Invalid public key algorithm"
