module Web.Rest.Media where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Lucid
import Network.Wai
import Web.Rest
import Web.Rest.ContentType
import Web.Rest.Status

json ::
    (?rest :: Rest) =>
    (FromJSON req, ToJSON res) =>
    (req -> ExceptT String IO res) ->
    IO Response
json runController =
    either badRequest pure =<< runExceptT make
  where
    make = do
        if ?rest.hasContentType ctApplicationJson
            then
                ok ctApplicationJson . encode
                    =<< runController
                    =<< (except . eitherDecode)
                    =<< lift ?rest.requestBody
            else
                unsupportedMediaType
                    ?rest.requestMethod
                    ctApplicationJson

html :: (Applicative a) => Html h -> a Response
html = ok ctApplicationHtml . renderBS
