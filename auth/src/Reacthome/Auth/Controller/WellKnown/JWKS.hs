module Reacthome.Auth.Controller.WellKnown.JWKS where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import JOSE.JWK
import JOSE.JWKS
import JOSE.PublicKey (makePublicKey)
import Reacthome.Auth.Domain.PublicKey (PublicKey, bytes, kid)
import Reacthome.Auth.Domain.PublicKeys (PublicKeys, getAll)
import Web.Rest
import Web.Rest.Media

jwks ::
    ( ?jwkPublicKeys :: PublicKeys
    ) =>
    ExceptT String IO Response
jwks = do
    keys <-
        traverse makeJWK
            =<< lift ?jwkPublicKeys.getAll
    toJSON $
        JWKS
            { keys
            }

makeJWK ::
    (Monad m) =>
    PublicKey ->
    ExceptT String m JWK
makeJWK key =
    toJWK <$> except (makePublicKey key.kid key.bytes)
