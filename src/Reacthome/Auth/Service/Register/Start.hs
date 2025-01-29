module Reacthome.Auth.Service.Register.Start where

import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.Challenges
import Reacthome.Auth.Service.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Service.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Service.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Service.WebAuthn.RegisterOptions

startRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    RegisterOptions ->
    IO (Either String PublicKeyCredentialCreationOptions)
startRegister =
    mkCreationOptions . validateRegisterOptions

mkCreationOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    Either String ValidRegisterOptions ->
    IO (Either String PublicKeyCredentialCreationOptions)
mkCreationOptions (Left err) = pure $ Left err
mkCreationOptions (Right options) = do
    uid <- mkRandomUserId
    challenge <- ?challenges.register options
    let user = mkPublicKeyCredentialUserEntity uid options
    pure . Right $
        mkPublicKeyCredentialCreationOptions
            mkPublicKeyCredentialRpEntity
            user
            challenge.value
            ?environment.timeout
