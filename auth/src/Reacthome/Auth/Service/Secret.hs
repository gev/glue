module Reacthome.Auth.Service.Secret where

import Data.Time.Clock.POSIX
import JOSE.KeyPair
import Reacthome.Auth.Domain.PublicKey (makePublicKey)
import Reacthome.Auth.Domain.PublicKeys

makeSecret :: (?jwkPublicKeys :: PublicKeys) => IO KeyPair
makeSecret = do
    kp <- generateKeyPair
    timestamp <- round <$> getPOSIXTime
    ?jwkPublicKeys.store $ makePublicKey kp timestamp
    pure kp
