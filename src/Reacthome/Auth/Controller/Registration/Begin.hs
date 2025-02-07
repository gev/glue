module Reacthome.Auth.Controller.Registration.Begin where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Registration.Options
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Domain.Registration.Begin
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenges
import Reacthome.Auth.Service.Registration.Begin

beginRegistration ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    ) =>
    RegistrationOptions ->
    ExceptT String IO PublicKeyCredentialCreationOptions
beginRegistration options = do
    login <- makeUserLogin options.login
    name <- makeUserName options.name
    makePublicKeyCredentialCreationOptions
        <$> runBeginRegistration
            BeginRegistration
                { login
                , name
                }
