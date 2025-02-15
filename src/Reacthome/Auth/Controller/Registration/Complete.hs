module Reacthome.Auth.Controller.Registration.Complete where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Registration.Registered
import Reacthome.Auth.Controller.WebAuthn.AuthenticatorAttestationResponse
import Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Registration.Complete
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Registration.Complete
import Util.Base64
import Util.Base64.URL qualified as URL
import Web.Rest
import Web.Rest.Media
import Web.Rest.Status

completeRegistration ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    IO Response
completeRegistration =
    either badRequest toJSON =<< runExceptT do
        credential <- fromJSON @(PublicKeyCredential AuthenticatorAttestationResponse) ?request
        cid <- makePublicKeyId <$> fromBase64 credential.id
        challenge <- makeChallenge <$> URL.fromBase64 credential.response.challenge
        publicKeyAlgorithm <- makePublicKeyAlgorithm credential.response.publicKeyAlgorithm
        publicKey <- fromBase64 credential.response.publicKey
        makeRegistered
            <$> runCompleteRegistration
                CompleteRegistration
                    { id = cid
                    , challenge
                    , publicKey
                    , publicKeyAlgorithm
                    }
