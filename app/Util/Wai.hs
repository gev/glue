module Util.Wai where

import Data.Aeson
import Data.ByteString
import Network.HTTP.Types
import Network.Wai

mkRespond ::
    (FromJSON req, ToJSON res) =>
    Request ->
    (Response -> IO ResponseReceived) ->
    (req -> IO (Maybe res)) ->
    IO ResponseReceived
mkRespond req respond run = do
    value <- decode <$> strictRequestBody req
    case value of
        (Just v) -> do
            res <- run v
            case res of
                Just r ->
                    respond $
                        responseLBS
                            status200
                            [(hContentType, ctApplicationJson)]
                            (encode r)
                Nothing -> respond $ invalidRequest
        Nothing -> respond $ invalidRequest

invalidRequest :: Response
invalidRequest =
    responseLBS
        status400
        [(hContentType, ctTextPlane)]
        "Invalid Request"

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
