module Reacthome.Auth.Service.Register.Start where

import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.Challenges

mkCreationOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    ) =>
    Either String User ->
    IO (Either String PublicKeyCredentialCreationOptions)
mkCreationOptions (Left err) = pure $ Left err
mkCreationOptions (Right user) = do
    isUserExists <- ?users.has user.login
    if isUserExists
        then
            pure . Left $ "User with login " <> show user.login.value <> " already exists"
        else do
            challenge <- ?challenges.register user
            let newUser = mkPublicKeyCredentialUserEntity user
            pure . Right $
                mkPublicKeyCredentialCreationOptions
                    mkPublicKeyCredentialRpEntity
                    newUser
                    challenge.value
                    ?environment.timeout
