module Util.Rest where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.CaseInsensitive
import Data.String
import Lucid
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai

ctTextPlane :: ByteString
ctTextPlane = "text/plain"

ctApplicationJson :: ByteString
ctApplicationJson = "application/json"

ctApplicationHtml :: ByteString
ctApplicationHtml = "text/html"

json ::
    (FromJSON req, ToJSON res) =>
    (req -> ExceptT String IO res) ->
    Request ->
    IO Response
json runController request =
    either badRequest pure =<< runExceptT make
  where
    make = do
        let contentType = lookup hContentType request.requestHeaders
        if contentType == Just ctApplicationJson
            then
                ok . encode
                    =<< runController
                    =<< (except . eitherDecode)
                    =<< lift (lazyRequestBody request)
            else
                unsupportedMediaType
                    request.requestMethod
                    ctApplicationJson

html :: (Applicative a) => Html h -> Request -> a Response
html = flip . const $ ok . renderBS

ok :: (Applicative a) => Lazy.ByteString -> a Response
ok =
    response
        status200
        [(hContentType, ctApplicationJson)]

badRequest :: (Applicative a) => String -> a Response
badRequest reason =
    response
        status400
        [(hContentType, ctTextPlane)]
        (fromString reason)

notFound :: (Applicative a) => a Response
notFound =
    response
        status404
        [(hContentType, ctTextPlane)]
        mempty

notAllowed :: (Applicative a) => Method -> a Response
notAllowed method =
    response
        status405
        [ (hContentType, ctTextPlane)
        , (hAllow, method)
        ]
        mempty

unsupportedMediaType :: (Applicative a) => Method -> ByteString -> a Response
unsupportedMediaType method mediaType =
    response
        status415
        [ (hContentType, ctTextPlane)
        , (mk $ original hAccept <> "-" <> method, mediaType)
        ]
        mempty

response :: (Applicative a) => Status -> ResponseHeaders -> Lazy.ByteString -> a Response
response status headers body = pure $ responseLBS status headers body

type Rest a c =
    (Applicative a) =>
    (c -> Request -> a Response) ->
    c ->
    Request ->
    a Response

get :: Rest a c
get = ifMethod methodGet

post :: Rest a c
post = ifMethod methodPost

ifMethod :: Method -> Rest a c
ifMethod method run controller request =
    if request.requestMethod == method
        then run controller request
        else notAllowed method
