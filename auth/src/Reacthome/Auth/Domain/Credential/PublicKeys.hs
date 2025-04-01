module Reacthome.Auth.Domain.Credential.PublicKeys where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.User.Id

data PublicKeys = PublicKeys
    { findById :: PublicKeyId -> MaybeT IO PublicKey
    , findByUserId :: UserId -> IO [PublicKey]
    , store :: PublicKey -> ExceptT String IO ()
    , remove :: PublicKeyId -> IO ()
    }
