module Service.Register.Finish where

import Environment
import Service.Challenge
import Service.WebAuthn.AuthenticatorAttestationResponse
import Service.WebAuthn.ClientDataJSON
import Service.WebAuthn.PublicKeyCredential
import Service.WebAuthn.RegisteredOptions

finishRegister ::
    ( ?environment :: Environment
    , ?challenges :: ChallengeSet
    ) =>
    EncodedPublicKeyCredential ->
    IO (Maybe RegisteredOptions)
finishRegister = do mkRegisteredOptions . decodedPublicKeyCredential

mkRegisteredOptions ::
    ( ?environment :: Environment
    , ?challenges :: ChallengeSet
    ) =>
    Maybe DecodedPublicKeyCredential ->
    IO (Maybe RegisteredOptions)
mkRegisteredOptions Nothing = pure Nothing
mkRegisteredOptions (Just credentials) = do
    let challenge = mkChallenge credentials.response.clientDataJSON.challenge
    isKnownChallenge <- ?challenges.has challenge
    if isKnownChallenge
        then do
            ?challenges.remove challenge
            pure $ Just (RegisteredOptions "Finish Register!")
        else
            pure Nothing
