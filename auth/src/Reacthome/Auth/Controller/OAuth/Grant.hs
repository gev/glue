module Reacthome.Auth.Controller.OAuth.Grant where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Data.ByteString
import Data.ByteString.Base64.URL (decodeUnpadded)
import Reacthome.Auth.Domain.Clients
import Reacthome.Auth.Service.OAuth.Grant
import Rest

getAuthorizationCode ::
    ( ?request :: Request
    , ?clients :: Clients
    ) =>
    ExceptT String IO ByteString
getAuthorizationCode = run grantAuthorizationCode "code"

getRefreshToken ::
    ( ?request :: Request
    , ?clients :: Clients
    ) =>
    ExceptT String IO ByteString
getRefreshToken = run grantRefreshToken "refresh_token"

run ::
    ( ?request :: Request
    , ?clients :: Clients
    ) =>
    (ByteString -> ExceptT String IO ()) ->
    ByteString ->
    ExceptT String IO ByteString
run grant name = do
    params <- except =<< lift ?request.bodyParams
    grant =<< except (params.lookup "grant_type")
    client_id <- except (params.lookup "client_id")
    client_secret <- except (params.lookup "client_secret")
    except =<< lift (grantClient client_id client_secret)
    except (decodeUnpadded =<< params.lookup name)
