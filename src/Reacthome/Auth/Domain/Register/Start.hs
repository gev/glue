module Reacthome.Auth.Domain.Register.Start where

import Data.Text
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name

data StartRegisterUser = StartRegisterUser
    { login :: UserLogin
    , name :: UserName
    }
    deriving stock (Show)

mkStartRegisterUser ::
    Text -> Text -> Either String StartRegisterUser
mkStartRegisterUser login name =
    StartRegisterUser
        <$> mkUserLogin login
        <*> mkUserName name
