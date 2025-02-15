module Web.Rest.Media where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Lucid
import Web.Rest
import Web.Rest.ContentType
import Web.Rest.Status

toJSON :: (ToJSON t, Applicative a) => t -> a Response
toJSON = ok ctApplicationJson mempty . encode

toHTML :: (Applicative a) => Html h -> a Response
toHTML = ok ctApplicationHtml mempty . renderBS

fromJSON :: (FromJSON t) => Request -> ExceptT String IO t
fromJSON request =
    if request.hasContentType ctApplicationJson
        then except . eitherDecode =<< lift request.body
        else throwE "Request should have `application/json` content type"
