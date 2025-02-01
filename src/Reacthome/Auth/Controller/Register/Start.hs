module Reacthome.Auth.Controller.Register.Start where

import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Controller.WebAuthn.RegisterOptions
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Register.Challenges
import Reacthome.Auth.Service.Register.Start

startRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    ) =>
    RegisterOptions ->
    IO (Either String PublicKeyCredentialCreationOptions)
startRegister options = do
    uid <- mkRandomUserId
    mkCreationOptions do
        login <- mkUserLogin options.name
        name <- mkUserName options.displayName
        pure $ mkNewUser uid login name
