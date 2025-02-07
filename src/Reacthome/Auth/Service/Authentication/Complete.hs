module Reacthome.Auth.Service.Authentication.Complete where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Authentication.Complete
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Challenges

runCompleteAuthentication ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    CompleteAuthentication ->
    ExceptT String IO User
runCompleteAuthentication credentials = do
    user <-
        maybeToExceptT
            ("Invalid challenge " <> show credentials.challenge.value)
            $ ?challenges.findBy credentials.challenge
    lift $ ?challenges.remove credentials.challenge
    _ <-
        maybeToExceptT ("Public key with id " <> show credentials.id.value <> " not found") $
            ?publicKeys.findById credentials.id

    -- publicKey' <-

    -- let isVerified = verify
    --     (HashSHA256)
    --     publicKey'
    --     credentials.message
    --     credentials.signature
    pure user
