module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm where

data PublicKeyAlgorithm
    = ED25519
    | ES256
    | RS256
    deriving stock (Show)
