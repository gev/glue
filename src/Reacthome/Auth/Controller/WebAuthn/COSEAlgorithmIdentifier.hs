module Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier where

import Control.Monad.Trans.Except
import Reacthome.Auth.Domain.Credential.PublicKey.Algorithm

type COSEAlgorithmIdentifier = Int

pattern ED25519' :: COSEAlgorithmIdentifier
pattern ED25519' = -8

pattern ES256' :: COSEAlgorithmIdentifier
pattern ES256' = -7

pattern RS256' :: COSEAlgorithmIdentifier
pattern RS256' = -257

mkPublicKeyAlgorithm ::
    (Monad m) =>
    COSEAlgorithmIdentifier ->
    ExceptT String m PublicKeyAlgorithm
mkPublicKeyAlgorithm = \case
    ED25519' -> pure ED25519
    ES256' -> pure ES256
    RS256' -> pure RS256
    _ -> throwE "Invalid public key algorithm"
