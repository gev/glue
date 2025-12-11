module Rest where

import Control.Exception
import Control.Monad
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
    , bodyParams :: IO (Either String BodyParams)
    , headers :: RequestHeaders
    , header :: HeaderName -> Maybe ByteString
    , query :: ByteString -> Maybe ByteString
    , hasContentType :: ContentType -> Bool
    , cookies :: Maybe Cookies
    , cookie :: ByteString -> Maybe ByteString
    }

data BodyParams = BodyParams
    { lookup :: ByteString -> Either String ByteString
    , list :: [Param]
    }

rest :: W.Request -> Request
rest request =
    let
        method = request.requestMethod
        body = W.lazyRequestBody request
        bodyParams =
            try @RequestParseException
                (parseRequestBody lbsBackEnd request)
                >>= \case
                    Left e -> pure . Left $ show e
                    Right r -> do
                        let list = fst r

                        let lookup' name = do
                                case Prelude.lookup name list of
                                    Nothing -> Left $ "Missing `" <> show name <> "` parameter"
                                    Just param -> Right param

                        pure . Right $
                            BodyParams lookup' list

        headers = request.requestHeaders
        header name = Prelude.lookup name headers
        query name = join $ Prelude.lookup name request.queryString
        hasContentType contentType = Just contentType == header hContentType
        cookies = parseCookies <$> header hCookie
        cookie name = Prelude.lookup name =<< cookies
     in
        Request{..}
