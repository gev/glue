module Rest.Media where

import Data.Aeson
import Lucid
import Rest
import Rest.ContentType
import Rest.Status

toJSON :: (ToJSON t, Applicative a) => t -> a Response
toJSON = ok ctApplicationJson mempty . encode

toHTML :: (Applicative a) => Html h -> a Response
toHTML = ok ctApplicationHtml mempty . renderBS

fromJSON :: (FromJSON t) => Request -> IO (Either String t)
fromJSON request =
    if request.hasContentType ctApplicationJson
        then eitherDecode <$> request.body
        else pure $ Left "Request should have `application/json` content type"
