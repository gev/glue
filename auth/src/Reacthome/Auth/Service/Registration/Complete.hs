module Reacthome.Auth.Service.Registration.Complete where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Registration.Complete
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers

runCompleteRegistration ::
    ( ?environment :: Environment
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?userPublicKeys :: PublicKeys
    ) =>
    CompleteRegistration ->
    ExceptT String IO User
runCompleteRegistration credentials = do
    user <-
        maybeToExceptT
            ("Invalid challenge " <> show credentials.challenge.value)
            $ ?authUsers.findBy credentials.challenge
    lift $ ?authUsers.remove credentials.challenge
    ?users.store user
    bytes <-
        decodePublicKey
            credentials.publicKeyAlgorithm
            credentials.publicKey
    ?userPublicKeys.store
        PublicKey
            { id = credentials.id
            , userId = user.id
            , algorithm = credentials.publicKeyAlgorithm
            , bytes
            }
    pure user
