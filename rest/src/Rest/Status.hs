module Rest.Status where

import Data.ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.CaseInsensitive
import Data.String
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai
import Rest.ContentType

ok :: (Applicative a) => ByteString -> ResponseHeaders -> Lazy.ByteString -> a Response
ok contentType headers =
    response
        status200
        ((hContentType, contentType) : headers)

redirect :: (Applicative a) => ByteString -> ResponseHeaders -> a Response
redirect location headers =
    response
        status302
        ((hLocation, location) : headers)
        mempty

badRequest :: (Applicative a) => String -> a Response
badRequest reason = do
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
response status headers body =
    pure $ responseLBS status headers body
