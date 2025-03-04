module Reacthome.Auth.Controller.WellKnown.JWKS where

import JOSE.JWKS
import JOSE.KeyPair
import Web.Rest
import Web.Rest.Media

jwks ::
    ( ?keyPair :: KeyPair
    , Applicative m
    ) =>
    m Response
jwks = toJSON $ JWKS{keys = [toJWK ?keyPair]}
