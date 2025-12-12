module Reacthome.Assist.Service.JOSE.PublicKey where

import Control.Concurrent
import Control.Error.Util (exceptT)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except)
import Data.Aeson
import JOSE.JWK
import JOSE.JWKS
import JOSE.PublicKey
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Reacthome.Assist.Environment

runPublicKeysUpdate ::
    ( ?environment :: Environment
    , ?publicKeys :: PublicKeys IO
    ) =>
    IO ()
runPublicKeysUpdate = do
    req <- parseRequest ?environment.jwksURL
    manager <- newManager tlsManagerSettings
    let updatePublicKeys = do
            {-
                TODO: Handle the HTTP response status code
            -}
            response <- lift $ responseBody <$> httpLbs req manager
            jwks <- except (eitherDecode @JWKS response)
            keys <- except (traverse fromJWK jwks.keys)
            lift (?publicKeys.store keys)

    void . forkIO . forever $ exceptT error pure do
        updatePublicKeys
        lift (threadDelay $ 1_000_000 * ?environment.publicKeysUpdateInterval)
