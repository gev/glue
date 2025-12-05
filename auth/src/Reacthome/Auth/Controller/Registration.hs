module Reacthome.Auth.Controller.Registration where

import Reacthome.Auth.View.Screen.Registration
import Rest
import Rest.Media

showRegistration :: (Applicative a) => a Response
showRegistration = toHTML registration
