module Reacthome.Auth.Service.OAuth.Grant where

import Control.Error ((??))
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString
import Data.ByteString.Base64
import Data.Text.Encoding
import Data.UUID
import Reacthome.Auth.Domain.Client
import Reacthome.Auth.Domain.Client.Id
import Reacthome.Auth.Domain.Clients
import Reacthome.Auth.Domain.Hash

grantClient ::
    (?clients :: Clients) =>
    ByteString -> ByteString -> IO (Either String ())
grantClient client_id client_secret = runExceptT do
    cid <- fromText (decodeUtf8 client_id) ?? "Invalid client id"
    client <- except (?clients.findById $ ClientId cid)
    secret <- except . decode $ client_secret
    assert "Client not authorized" do
        client.secret == makeHash secret

grantAuthorizationCode :: ByteString -> ExceptT String IO ()
grantAuthorizationCode grant_type =
    assert "Invalid grant type" $
        grant_type == "authorization_code"

grantRefreshToken :: ByteString -> ExceptT String IO ()
grantRefreshToken grant_type =
    assert "Invalid grant type" $
        grant_type == "refresh_token"

assert :: e -> Bool -> ExceptT e IO ()
assert err p = unless p (throwE err)
