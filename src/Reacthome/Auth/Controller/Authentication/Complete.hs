module Reacthome.Auth.Controller.Authentication.Complete where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Controller.AuthFlowCookie
import Reacthome.Auth.Controller.Authentication.Authenticated
import Reacthome.Auth.Controller.WebAuthn.AuthenticatorAssertionResponse
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Domain.Authentication.Complete
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.AuthFlows
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Authentication.Complete
import Reacthome.Auth.Service.Challenge
import Util.Base64
import Util.Base64.URL qualified as URL
import Web.Rest
import Web.Rest.Media
import Web.Rest.Status

completeAuthentication ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authFlows :: AuthFlows
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    IO Response
completeAuthentication =
    either badRequest toJSON =<< runExceptT do
        authFlow <- maybeToExceptT "Invalid auth flow" . ?authFlows.findBy =<< getAuthFlowCookie
        credential <- fromJSON @(PublicKeyCredential AuthenticatorAssertionResponse) ?request
        user <- authenticateBy credential
        case authFlow of
            AuthCodeGrant{} -> throwE "Invalid auth flow"
            CredentialGrant -> pure $ makeAuthenticated user
  where
    authenticateBy credential = do
        cid <- makePublicKeyId <$> fromBase64 credential.id
        challenge <- makeChallenge <$> URL.fromBase64 credential.response.challenge
        message <- fromBase64 credential.response.message
        signature <- fromBase64 credential.response.signature
        runCompleteAuthentication
            CompleteAuthentication
                { id = cid
                , challenge
                , message
                , signature
                }
