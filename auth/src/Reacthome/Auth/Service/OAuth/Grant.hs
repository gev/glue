module Reacthome.Auth.Service.OAuth.Grant where

import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString

grantClient :: ByteString -> ByteString -> ExceptT String IO ()
grantClient client_id client_secret =
    assert "Invalid client" $
        client_id == "reacthome" && client_secret == "reacthome"

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
