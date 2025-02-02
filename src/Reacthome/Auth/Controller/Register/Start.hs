module Reacthome.Auth.Controller.Register.Start where

import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Controller.WebAuthn.RegisterOptions

import Control.Monad.Trans.Except
import Reacthome.Auth.Domain.Register.Start
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
    ExceptT String IO PublicKeyCredentialCreationOptions
startRegister options = do
    command <- except $ mkStartRegisterUser options.login options.name
    preRegisteredUser <- runStartRegister command
    pure $ mkPublicKeyCredentialCreationOptions preRegisteredUser
