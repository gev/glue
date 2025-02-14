module Web.Rest.Media where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Lucid
import Network.HTTP.Types
import Network.Wai
import Web.Rest.ContentType
import Web.Rest.Status

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
                ok ctApplicationJson . encode
                    =<< runController
                    =<< (except . eitherDecode)
                    =<< lift (lazyRequestBody request)
            else
                unsupportedMediaType
                    request.requestMethod
                    ctApplicationJson

html :: (Applicative a) => Html h -> Request -> a Response
html = flip . const $ ok ctApplicationHtml . renderBS
