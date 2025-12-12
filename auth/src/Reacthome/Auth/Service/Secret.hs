module Reacthome.Auth.Service.Secret where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (except)
import Data.Time.Clock.POSIX
import JOSE.KeyPair
import Reacthome.Auth.Domain.PublicKey (makePublicKey)
import Reacthome.Auth.Domain.PublicKeys

makeSecret :: (?jwkPublicKeys :: PublicKeys) => IO KeyPair
makeSecret = exceptT error pure do
    kp <- lift generateKeyPair
    timestamp <- lift (round <$> getPOSIXTime)
    except =<< lift (?jwkPublicKeys.store $ makePublicKey kp timestamp)
    pure kp
