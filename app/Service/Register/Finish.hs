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
    IO (Maybe RegisteredOptions)
finishRegister = do mkRegisteredOptions . decodedPublicKeyCredential

mkRegisteredOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    ) =>
    Maybe DecodedPublicKeyCredential ->
    IO (Maybe RegisteredOptions)
mkRegisteredOptions Nothing = pure Nothing
mkRegisteredOptions (Just credentials) = do
    let challenge = mkChallenge credentials.response.clientDataJSON.challenge
    options <- ?challenges.get challenge
    case options of
        Nothing -> pure Nothing
        Just _ -> do
            ?challenges.remove challenge
            print credentials
            pure $ Just (RegisteredOptions "Finish Register!")
