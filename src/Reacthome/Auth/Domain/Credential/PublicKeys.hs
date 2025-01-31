module Reacthome.Auth.Domain.Credential.PublicKeys where

import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.User.Id

data PublicKeys = PublicKeys
    { findById :: PublicKeyId -> IO (Maybe PublicKey)
    , findByUserId :: UserId -> IO (Maybe [PublicKey])
    , store :: PublicKey -> IO (Either String ())
    , remove :: PublicKeyId -> IO ()
    }
