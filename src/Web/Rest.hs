module Web.Rest where

import Control.Monad
import Data.ByteString
import Data.ByteString.Lazy qualified as Lazy
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai qualified as W
import Web.Rest.ContentType

type Response = W.Response

data Request = Request
    { method :: Method
    , body :: IO Lazy.ByteString
    , header :: HeaderName -> Maybe ContentType
    , query :: ByteString -> Maybe ByteString
    , hasContentType :: ContentType -> Bool
    }

rest :: W.Request -> Request
rest request =
    Request
        { method
        , body
        , header
        , query
        , hasContentType
        }
  where
    method = W.requestMethod request
    body = W.lazyRequestBody request
    header name = lookup name $ W.requestHeaders request
    query name = join $ lookup name $ W.queryString request
    hasContentType contentType =
        Just contentType == header hContentType
