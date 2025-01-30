module Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier where

type COSEAlgorithmIdentifier = Int

ed25519 :: COSEAlgorithmIdentifier
ed25519 = -8

es256 :: COSEAlgorithmIdentifier
es256 = -7

rs256 :: COSEAlgorithmIdentifier
rs256 = -257
