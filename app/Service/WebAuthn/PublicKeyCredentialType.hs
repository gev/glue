module Service.WebAuthn.PublicKeyCredentialType where

import Data.Text

type PublicKeyCredentialType = Text

publicKeyCredentialType :: PublicKeyCredentialType
publicKeyCredentialType = "public-key"
