module Reacthome.Auth.Service.Authenticate.Finish where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Authenticate.Finish
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Challenges

runFinishAuthenticate ::
    ( ?environment :: Environment
    , ?challenges :: Challenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    FinishAuthenticate ->
    ExceptT String IO User
runFinishAuthenticate credentials = do
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
