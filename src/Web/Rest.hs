module Web.Rest where

import Data.ByteString.Lazy qualified as Lazy
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai qualified as W
import Web.Rest.ContentType

type Response = W.Response

data Rest = Rest
    { requestMethod :: Method
    , requestBody :: IO Lazy.ByteString
    , requestHeader :: HeaderName -> Maybe ContentType
    , hasContentType :: ContentType -> Bool
    }

rest :: W.Request -> Rest
rest request =
    Rest
        { requestMethod
        , requestBody
        , requestHeader
        , hasContentType
        }
  where
    requestMethod = W.requestMethod request
    requestBody = W.lazyRequestBody request
    requestHeader name = lookup name (W.requestHeaders request)
    hasContentType contentType =
        Just contentType == requestHeader hContentType
