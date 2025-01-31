module Reacthome.Auth.Domain.Credential.PublicKeys where

import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.User.Id

data PublicKeys = PublicKeys
    { getById :: PublicKeyId -> IO (Either String PublicKey)
    , getByUserId :: UserId -> IO (Either String [PublicKey])
    , store :: PublicKey -> IO (Either String ())
    , remove :: PublicKeyId -> IO ()
    }
