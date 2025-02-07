module Reacthome.Auth.Controller.Register.Begin where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Register.RegisterOptions
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Domain.Register.Begin
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenges
import Reacthome.Auth.Service.Register.Begin

beginRegister ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    ) =>
    RegisterOptions ->
    ExceptT String IO PublicKeyCredentialCreationOptions
beginRegister options = do
    login <- mkUserLogin options.login
    name <- mkUserName options.name
    mkPublicKeyCredentialCreationOptions
        <$> runBeginRegister
            BeginRegister
                { login
                , name
                }
