module Reacthome.Auth.Controller.Authentication where

import Reacthome.Auth.View.Screen.Authentication
import Rest
import Rest.Media

showAuthentication :: (Applicative a) => a Response
showAuthentication = toHTML authentication
