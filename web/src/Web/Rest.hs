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
import Network.Wai.Parse
import Web.Cookie
import Web.Rest.ContentType

type Response = W.Response

data Request
    = Request
    { method :: Method
    , body :: IO Lazy.ByteString
    , bodyParams :: ExceptT String IO [Param]
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
    method = request.requestMethod
    body = W.lazyRequestBody request
    bodyParams =
        withExceptT show . except
            =<< lift do
                try @RequestParseException $
                    fst <$> parseRequestBody lbsBackEnd request

    headers = request.requestHeaders
    header name = lookup name headers
    query name = join $ lookup name request.queryString
    hasContentType contentType = Just contentType == header hContentType
    cookies = parseCookies <$> header hCookie
    cookie name = lookup name =<< cookies
