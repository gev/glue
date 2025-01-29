module Reacthome.Auth.Service.Register.Finish where

import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.Challenges
import Reacthome.Auth.Service.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Service.WebAuthn.RegisteredOptions

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
    case options of
        Left err -> pure $ Left err
        Right _ -> do
            ?challenges.remove challenge
            pure $ Right (RegisteredOptions "Finish Register!")
