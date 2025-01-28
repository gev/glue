module Service.Register.Start where

import Crypto.Random
import Data.ByteString
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
    uid <- getRandomBytes 20
    challenge <- ?challenges.get
    let user = mkUserEntity uid options
    print options
    let res =
            mkPublicKeyCredentialCreationOptions
                mkRpEntity
                user
                challenge.value
                ?environment.timeout
    print res
    pure . Just $ res

mkUserEntity ::
    ByteString ->
    ValidRegisterOptions ->
    PublicKeyCredentialUserEntity
mkUserEntity uid valid =
    mkPublicKeyCredentialUserEntity
        uid
        valid.options.name
        valid.options.displayName

mkRpEntity ::
    (?environment :: Environment) =>
    PublicKeyCredentialRpEntity
mkRpEntity =
    PublicKeyCredentialRpEntity
        { id = Just ?environment.domain
        , name = ?environment.name
        }
