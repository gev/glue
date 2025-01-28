module Service.Register.Start where

import Environment
import Service.Challenge
import Service.Register.Challenges
import Service.WebAuthn.PublicKeyCredentialCreationOptions
import Service.WebAuthn.PublicKeyCredentialRpEntity
import Service.WebAuthn.PublicKeyCredentialUserEntity
import Service.WebAuthn.RegisterOptions

startRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    RegisterOptions ->
    IO (Maybe PublicKeyCredentialCreationOptions)
startRegister =
    mkCreationOptions . validateRegisterOptions

mkCreationOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    Maybe ValidRegisterOptions ->
    IO (Maybe PublicKeyCredentialCreationOptions)
mkCreationOptions Nothing = pure Nothing
mkCreationOptions (Just options) = do
    uid <- mkRandomUserId
    challenge <- ?challenges.register options
    let user = mkPublicKeyCredentialUserEntity uid options
    pure . Just $
        mkPublicKeyCredentialCreationOptions
            mkPublicKeyCredentialRpEntity
            user
            challenge.value
            ?environment.timeout
