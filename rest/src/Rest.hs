module Rest where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.ByteString.Lazy qualified as Lazy
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai qualified as W
import Network.Wai.Parse
import Rest.ContentType
import Web.Cookie

type Response = W.Response

data Request
    = Request
    { method :: Method
    , body :: IO Lazy.ByteString
    , bodyParams :: ExceptT String IO BodyParams
    , headers :: RequestHeaders
    , header :: HeaderName -> Maybe ByteString
    , query :: ByteString -> Maybe ByteString
    , hasContentType :: ContentType -> Bool
    , cookies :: Maybe Cookies
    , cookie :: ByteString -> Maybe ByteString
    }

data BodyParams = BodyParams
    { lookup :: ByteString -> ExceptT String IO ByteString
    , list :: [Param]
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
    bodyParams = do
        list <-
            withExceptT show . except
                =<< lift do
                    try @RequestParseException $
                        fst <$> parseRequestBody lbsBackEnd request
        let lookup' name =
                maybeToExceptT ("Missing `" <> show name <> "` parameter")
                    . hoistMaybe
                    $ Prelude.lookup name list
        pure $
            BodyParams lookup' list

    headers = request.requestHeaders
    header name = Prelude.lookup name headers
    query name = join $ Prelude.lookup name request.queryString
    hasContentType contentType = Just contentType == header hContentType
    cookies = parseCookies <$> header hCookie
    cookie name = Prelude.lookup name =<< cookies
