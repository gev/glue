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
import Web.Rest
import Web.Rest.Media

beginRegistration ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    ) =>
    IO Response
beginRegistration = json run
  where
    run ::
        RegistrationOptions ->
        ExceptT String IO PublicKeyCredentialCreationOptions
    run options = do
        login <- makeUserLogin options.login
        name <- makeUserName options.name
        makePublicKeyCredentialCreationOptions
            <$> runBeginRegistration
                BeginRegistration
                    { login
                    , name
                    }
