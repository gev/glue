module Util.Wai where

import Data.Aeson
import Data.ByteString
import Data.String
import Network.HTTP.Types
import Network.Wai

mkRespond ::
    (FromJSON req, ToJSON res) =>
    Request ->
    (Response -> IO ResponseReceived) ->
    (req -> IO (Either String res)) ->
    IO ResponseReceived
mkRespond req respond run = do
    let contentType = lookup hContentType req.requestHeaders
    if contentType == Just ctApplicationJson
        then do
            value <- eitherDecode <$> lazyRequestBody req
            case value of
                (Right v) -> do
                    res <- run v
                    respond case res of
                        Right content -> ok content
                        Left err -> badRequest err
                (Left err) -> respond $ badRequest err
        else respond $ badRequest "Content-Type is not application/json"

ok :: (ToJSON a) => a -> Response
ok content =
    responseLBS
        status200
        [(hContentType, ctApplicationJson)]
        (encode content)

badRequest :: String -> Response
badRequest reason =
    responseLBS
        status400
        [(hContentType, ctTextPlane)]
        ("Bad Request. " <> fromString reason)

notAllowed :: Response
notAllowed =
    responseLBS
        status405
        [(hContentType, ctTextPlane)]
        "Method Not Allowed"

ctTextPlane :: ByteString
ctTextPlane = "text/plain"

ctApplicationJson :: ByteString
ctApplicationJson = "application/json"
