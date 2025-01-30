module Reacthome.Auth.Controller.Register.Start where

import Reacthome.Auth.Controller.Challenge
import Reacthome.Auth.Controller.Register.Challenges
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Controller.WebAuthn.RegisterOptions
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.UserId
import Reacthome.Auth.Domain.UserLogin
import Reacthome.Auth.Domain.UserName
import Reacthome.Auth.Environment

startRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    RegisterOptions ->
    IO (Either String PublicKeyCredentialCreationOptions)
startRegister options = do
    uid <- mkRandomUserId
    mkCreationOptions do
        login <- mkUserLogin options.name
        name <- mkUserName options.displayName
        pure $ mkNewUser uid login name

mkCreationOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    Either String User ->
    IO (Either String PublicKeyCredentialCreationOptions)
mkCreationOptions (Left err) = pure $ Left err
mkCreationOptions (Right user) = do
    challenge <- ?challenges.register user
    let user' = mkPublicKeyCredentialUserEntity user
    pure . Right $
        mkPublicKeyCredentialCreationOptions
            mkPublicKeyCredentialRpEntity
            user'
            challenge.value
            ?environment.timeout
