module Reacthome.Assist.Service.JOSE.PublicKey where

import Control.Concurrent
import Control.Monad
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
            response <- responseBody <$> httpLbs req manager
            either
                print
                ?publicKeys.store
                do
                    jwks <- eitherDecode @JWKS response
                    traverse fromJWK jwks.keys
    void . forkIO . forever $ do
        updatePublicKeys
        threadDelay $ 1_000_000 * ?environment.publicKeysUpdateInterval
