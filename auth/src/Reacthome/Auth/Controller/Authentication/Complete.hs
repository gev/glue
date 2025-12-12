module Reacthome.Auth.Controller.Authentication.Complete where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.AuthFlowCookie
import Reacthome.Auth.Controller.Authentication.Authenticated
import Reacthome.Auth.Controller.WebAuthn.AuthenticatorAssertionResponse
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Domain.Authentication.Complete
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthFlows
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Authentication.Complete
import Rest
import Rest.Media
import Rest.Status (badRequest)
import Util.Base64
import Util.Base64.URL qualified as URL

completeAuthentication ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authFlows :: AuthFlows
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?userPublicKeys :: PublicKeys
    ) =>
    IO Response
completeAuthentication = exceptT badRequest toJSON do
    cookie <- getAuthFlowCookie
    authFlow <- except =<< lift (?authFlows.findBy cookie)
    credential <- except =<< lift (fromJSON @(PublicKeyCredential AuthenticatorAssertionResponse) ?request)
    user <- authenticateBy credential
    lift (makeAuthenticated authFlow user)
  where
    authenticateBy credential = do
        cid <- except (makePublicKeyId <$> fromBase64 credential.id)
        challenge <- except (makeChallenge <$> URL.fromBase64 credential.response.challenge)
        message <- except (fromBase64 credential.response.message)
        signature <- except (fromBase64 credential.response.signature)
        except =<< lift (runCompleteAuthentication CompleteAuthentication{id = cid, ..})
