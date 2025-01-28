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
    value <- decode <$> lazyRequestBody req
    case value of
        (Just v) -> do
            res <- run v
            respond case res of
                Right content -> ok content
                Left err -> badRequest err
        Nothing -> respond $ badRequest "Body is not a valid JSON"

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
