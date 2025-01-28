module Service.Register.Finish where

import Environment
import Service.Challenge
import Service.Register.Challenges
import Service.WebAuthn.AuthenticatorAttestationResponse
import Service.WebAuthn.ClientDataJSON
import Service.WebAuthn.PublicKeyCredential
import Service.WebAuthn.RegisteredOptions

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
    let challenge = mkChallenge credentials.response.clientDataJSON.challenge
    options <- ?challenges.get challenge
    case options of
        Left err -> pure $ Left err
        Right _ -> do
            ?challenges.remove challenge
            print credentials
            pure $ Right (RegisteredOptions "Finish Register!")
