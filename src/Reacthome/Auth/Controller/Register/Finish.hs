module Reacthome.Auth.Controller.Register.Finish where

import Reacthome.Auth.Controller.Challenge
import Reacthome.Auth.Controller.Register.Challenges
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Controller.WebAuthn.RegisteredOptions
import Reacthome.Auth.Environment

finishRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    EncodedPublicKeyCredential ->
    IO (Either String RegisteredOptions)
finishRegister = mkRegisteredOptions . decodePublicKeyCredential

mkRegisteredOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    Either String DecodedPublicKeyCredential ->
    IO (Either String RegisteredOptions)
mkRegisteredOptions (Left err) = pure $ Left err
mkRegisteredOptions (Right credentials) = do
    let challenge = mkChallenge credentials.challenge
    options <- ?challenges.get challenge
    print credentials
    case options of
        Nothing -> pure $ Left "Challenge not found"
        Just user -> do
            ?challenges.remove challenge
            pure $
                Right
                    RegisteredOptions
                        { rp = mkPublicKeyCredentialRpEntity
                        , user = mkPublicKeyCredentialUserEntity user
                        }
