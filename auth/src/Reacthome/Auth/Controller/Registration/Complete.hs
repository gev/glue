module Reacthome.Auth.Controller.Registration.Complete where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Registration.Registered
import Reacthome.Auth.Controller.WebAuthn.AuthenticatorAttestationResponse
import Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Registration.Complete
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Registration.Complete
import Rest
import Rest.Media
import Rest.Status (badRequest)
import Util.Base64
import Util.Base64.URL qualified as URL

completeRegistration ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?userPublicKeys :: PublicKeys
    ) =>
    IO Response
completeRegistration = exceptT badRequest toJSON do
    credential <- except =<< lift (fromJSON @(PublicKeyCredential AuthenticatorAttestationResponse) ?request)
    cid <- except (makePublicKeyId <$> fromBase64 credential.id)
    challenge <- except (makeChallenge <$> URL.fromBase64 credential.response.challenge)
    publicKeyAlgorithm <- makePublicKeyAlgorithm credential.response.publicKeyAlgorithm
    publicKey <- except (fromBase64 credential.response.publicKey)
    completedRegistration <- except =<< lift (runCompleteRegistration CompleteRegistration{id = cid, ..})
    pure (makeRegistered completedRegistration)
