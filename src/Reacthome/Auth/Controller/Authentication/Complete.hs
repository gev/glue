module Reacthome.Auth.Controller.Authentication.Complete where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Authentication.Authenticated
import Reacthome.Auth.Controller.WebAuthn.AuthenticatorAssertionResponse
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Domain.Authentication.Complete
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Authentication.Complete
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Challenges
import Util.Base64

completeAuthentication ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    PublicKeyCredential AuthenticatorAssertionResponse ->
    ExceptT String IO Authenticated
completeAuthentication credential = do
    cid <- makePublicKeyId <$> fromBase64 credential.id
    challenge <- makeChallenge <$> fromBase64 credential.response.challenge
    message <- fromBase64 credential.response.message
    signature <- fromBase64 credential.response.signature
    makeAuthenticated
        <$> runCompleteAuthentication
            CompleteAuthentication
                { id = cid
                , challenge
                , message
                , signature
                }
