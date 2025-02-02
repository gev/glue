module Util.Wai where

import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString
import Data.String
import Network.HTTP.Types
import Network.Wai

mkRespond ::
    (FromJSON req, ToJSON res) =>
    Request ->
    (Response -> IO ResponseReceived) ->
    (req -> ExceptT String IO res) ->
    IO ResponseReceived
mkRespond req respond runController = do
    let contentType = lookup hContentType req.requestHeaders
    if contentType == Just ctApplicationJson
        then do
            json <- lazyRequestBody req
            either
                (respond . badRequest)
                ( runExceptT . runController
                    >=> either
                        (respond . badRequest)
                        (respond . ok)
                )
                (eitherDecode json)
        else
            respond $
                badRequest "Content-Type is not application/json"

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
