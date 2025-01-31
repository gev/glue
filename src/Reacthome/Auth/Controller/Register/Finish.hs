module Reacthome.Auth.Controller.Register.Finish where

import Reacthome.Auth.Controller.Challenge
import Reacthome.Auth.Controller.Register.Challenges
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Controller.WebAuthn.RegisteredOptions
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment

finishRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    EncodedPublicKeyCredential ->
    IO (Either String RegisteredOptions)
finishRegister = mkRegisteredOptions . decodePublicKeyCredential

mkRegisteredOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    Either String DecodedPublicKeyCredential ->
    IO (Either String RegisteredOptions)
mkRegisteredOptions (Left err) = pure $ Left err
mkRegisteredOptions (Right credentials) = do
    let challenge = mkChallenge credentials.challenge
    user' <- ?challenges.findBy challenge
    ?challenges.remove challenge
    case user' of
        Just user -> do
            success <- ?users.store user
            case success of
                Left err -> pure $ Left err
                _ -> do
                    success' <-
                        ?publicKeys.store $
                            PublicKey
                                { id = PublicKeyId credentials.id
                                , userId = user.id
                                , algorithm = credentials.publicKeyAlgorithm
                                , bytes = credentials.publicKey
                                }
                    case success' of
                        Left err -> pure $ Left err
                        _ ->
                            pure . Right $
                                RegisteredOptions
                                    { rp = mkPublicKeyCredentialRpEntity
                                    , user = mkPublicKeyCredentialUserEntity user
                                    }
        _ -> pure . Left $ "Invalid challenge " <> show challenge.value
