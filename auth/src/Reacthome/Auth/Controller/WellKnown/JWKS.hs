module Reacthome.Auth.Controller.WellKnown.JWKS where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import JOSE.JWK
import JOSE.JWKS
import JOSE.PublicKey (makePublicKey)
import Reacthome.Auth.Domain.PublicKey (PublicKey, bytes, kid)
import Reacthome.Auth.Domain.PublicKeys (PublicKeys, getAll)
import Rest
import Rest.Media
import Rest.Status (badRequest)

jwks ::
    (?jwkPublicKeys :: PublicKeys) =>
    IO Response
jwks = exceptT badRequest toJSON do
    pks <- except =<< lift ?jwkPublicKeys.getAll
    keys <- except (traverse makeJWK pks)
    pure JWKS{..}

makeJWK :: PublicKey -> Either String JWK
makeJWK key = toJWK <$> makePublicKey key.kid key.bytes
