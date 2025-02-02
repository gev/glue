module Reacthome.Auth.Service.Register.Finish where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
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
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.Challenges

mkRegisteredOptions ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    Either String DecodedPublicKeyCredential ->
    ExceptT String IO RegisteredOptions
mkRegisteredOptions (Left err) = throwE err
mkRegisteredOptions (Right credentials) = do
    let challenge = mkChallenge credentials.challenge
    user <-
        maybeToExceptT
            ("Invalid challenge " <> show challenge.value)
            (?challenges.findBy challenge)
    lift $ ?challenges.remove challenge
    ?users.store user
    ?publicKeys.store
        PublicKey
            { id = PublicKeyId credentials.id
            , userId = user.id
            , algorithm = credentials.publicKeyAlgorithm
            , bytes = credentials.publicKey
            }
    pure $
        RegisteredOptions
            { rp = mkPublicKeyCredentialRpEntity
            , user = mkPublicKeyCredentialUserEntity user
            }
