module Reacthome.Auth.Service.OAuth.Grant where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.ByteString.Base64
import Data.Text.Encoding
import Data.UUID
import Reacthome.Auth.Domain.Client
import Reacthome.Auth.Domain.Client.Id
import Reacthome.Auth.Domain.Clients
import Reacthome.Auth.Domain.Hash

grantClient :: (?clients :: Clients) => ByteString -> ByteString -> ExceptT String IO ()
grantClient client_id client_secret = do
    cid <-
        maybeToExceptT "Invalid client id"
            . hoistMaybe
            . fromText
            . decodeUtf8
            $ client_id
    client <- maybeToExceptT "Unknown client" $ ?clients.findById $ ClientId cid
    secret <- except . decode $ client_secret
    assert "Client not authorized" $
        client.secret == makeHash secret

grantAuthorizationCode :: ByteString -> ExceptT String IO ()
grantAuthorizationCode grant_type =
    assert "Invalid grant type" $
        grant_type == "authorization_code"

grantRefreshToken :: ByteString -> ExceptT String IO ()
grantRefreshToken grant_type =
    assert "Invalid grant type" $
        grant_type == "refresh_token"

assert :: (Monad m) => e -> Bool -> ExceptT e m ()
assert err p = unless p $ throwE err
