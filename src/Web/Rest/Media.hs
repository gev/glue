module Web.Rest.Media where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Lucid
import Web.Rest
import Web.Rest.ContentType
import Web.Rest.Status

json ::
    (?request :: Request) =>
    (FromJSON req, ToJSON res) =>
    (req -> ExceptT String IO res) ->
    IO Response
json runController =
    either badRequest pure =<< runExceptT make
  where
    make = do
        if ?request.hasContentType ctApplicationJson
            then
                ok ctApplicationJson mempty . encode
                    =<< runController
                    =<< (except . eitherDecode)
                    =<< lift ?request.body
            else
                unsupportedMediaType
                    ?request.method
                    ctApplicationJson

html :: (Applicative a) => Html h -> a Response
html = ok ctApplicationHtml mempty . renderBS
