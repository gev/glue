module Reacthome.Auth.Controller.Register.Start where

import Reacthome.Auth.Controller.Challenge
import Reacthome.Auth.Controller.Register.Challenges
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Controller.WebAuthn.RegisterOptions
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment

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

mkCreationOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    ) =>
    Either String User ->
    IO (Either String PublicKeyCredentialCreationOptions)
mkCreationOptions (Left err) = pure $ Left err
mkCreationOptions (Right user) = do
    existedUser <- ?users.getByLogin user.login
    case existedUser of
        Right _ -> pure . Left $ "User with login " <> show user.login.value <> " already exists"
        Left _ -> do
            challenge <- ?challenges.register user
            let newUser = mkPublicKeyCredentialUserEntity user
            pure . Right $
                mkPublicKeyCredentialCreationOptions
                    mkPublicKeyCredentialRpEntity
                    newUser
                    challenge.value
                    ?environment.timeout
