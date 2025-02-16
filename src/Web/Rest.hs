module Web.Rest where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString
import Data.ByteString.Lazy qualified as Lazy
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai qualified as W
import Network.Wai.Parse qualified as W
import Web.Cookie
import Web.Rest.ContentType

type Response = W.Response

data Request
    = Request
    { method :: Method
    , body :: IO Lazy.ByteString
    , bodyParams :: ExceptT W.RequestParseException IO ([W.Param], [W.File Lazy.ByteString])
    , headers :: RequestHeaders
    , header :: HeaderName -> Maybe ContentType
    , query :: ByteString -> Maybe ByteString
    , hasContentType :: ContentType -> Bool
    , cookies :: Maybe Cookies
    , cookie :: ByteString -> Maybe ByteString
    }

rest :: W.Request -> Request
rest request =
    Request
        { method
        , body
        , bodyParams
        , headers
        , header
        , query
        , hasContentType
        , cookies
        , cookie
        }
  where
    method = W.requestMethod request
    body = W.lazyRequestBody request
    bodyParams = except =<< lift (try $ W.parseRequestBody W.lbsBackEnd request)
    headers = W.requestHeaders request
    header name = lookup name headers
    query name = join $ lookup name $ W.queryString request
    hasContentType contentType = Just contentType == header hContentType
    cookies = parseCookies <$> header hCookie
    cookie name = lookup name =<< cookies
