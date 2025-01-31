module Reacthome.Auth.Domain.Credential.PublicKeys where

import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Id

data PublicKeys = PublicKeys
    { get :: PublicKeyId -> IO (Either String PublicKey)
    , store :: PublicKey -> IO (Either String ())
    , remove :: PublicKeyId -> IO ()
    }
