module Reacthome.Auth.Controller.Authenticate.Finish where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Authenticate.AuthenticatedUser
import Reacthome.Auth.Controller.WebAuthn.AuthenticatorAssertionResponse
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Domain.Authenticate.Finish
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Authenticate.Finish
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Challenges
import Util.Base64

finishAuthenticate ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    PublicKeyCredential AuthenticatorAssertionResponse ->
    ExceptT String IO AuthenticatedUser
finishAuthenticate credential = do
    cid <- mkPublicKeyId <$> fromBase64 credential.id
    challenge <- mkChallenge <$> fromBase64 credential.response.challenge
    message <- fromBase64 credential.response.message
    signature <- fromBase64 credential.response.signature
    mkAuthenticatedUser
        <$> runFinishAuthenticate
            FinishAuthenticate
                { id = cid
                , challenge
                , message
                , signature
                }
