module Reacthome.Auth.Controller.Authenticate.Finish where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Register.RegisteredUser
import Reacthome.Auth.Controller.WebAuthn.COSEAlgorithmIdentifier
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Register.Finish
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Challenges
import Reacthome.Auth.Service.Register.Finish
import Util.Base64

finishAuthenticate ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    PublicKeyCredential ->
    ExceptT String IO RegisteredUser
finishAuthenticate credential = do
    cid <- mkPublicKeyId <$> fromBase64 credential.id
    challenge <- mkChallenge <$> fromBase64 credential.challenge
    publicKey <- fromBase64 credential.publicKey
    publicKeyAlgorithm <- mkPublicKeyAlgorithm credential.publicKeyAlgorithm
    mkRegisteredOptions
        <$> runFinishRegister
            FinishRegister
                { id = cid
                , challenge
                , publicKey
                , publicKeyAlgorithm
                }
