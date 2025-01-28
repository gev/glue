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
