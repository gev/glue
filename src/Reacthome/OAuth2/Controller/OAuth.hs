module Reacthome.OAuth2.Controller.OAuth where

import Web.Rest
import Web.Rest.Status

oauth :: (Applicative a) => a Response
oauth = redirect mempty "/authentication"
