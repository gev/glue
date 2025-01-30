module Reacthome.Auth.Controller.Register.Finish where

import Reacthome.Auth.Controller.Challenge
import Reacthome.Auth.Controller.Register.Challenges
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Controller.WebAuthn.RegisteredOptions
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment

finishRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    ) =>
    EncodedPublicKeyCredential ->
    IO (Either String RegisteredOptions)
finishRegister = mkRegisteredOptions . decodePublicKeyCredential

mkRegisteredOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    ) =>
    Either String DecodedPublicKeyCredential ->
    IO (Either String RegisteredOptions)
mkRegisteredOptions (Left err) = pure $ Left err
mkRegisteredOptions (Right credentials) = do
    let challenge = mkChallenge credentials.challenge
    user' <- ?challenges.get challenge
    ?challenges.remove challenge
    case user' of
        Left err -> pure $ Left err
        Right user -> do
            success <- ?users.store user
            pure case success of
                Left err -> Left err
                _ ->
                    Right
                        RegisteredOptions
                            { rp = mkPublicKeyCredentialRpEntity
                            , user = mkPublicKeyCredentialUserEntity user
                            }
