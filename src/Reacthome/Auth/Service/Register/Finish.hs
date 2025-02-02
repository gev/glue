module Reacthome.Auth.Service.Register.Finish where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Register.Finish
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.Register.Challenges

runFinishRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    FinishRegister ->
    ExceptT String IO User
runFinishRegister credentials = do
    user <-
        maybeToExceptT
            ("Invalid challenge " <> show credentials.challenge.value)
            $ ?challenges.findBy credentials.challenge
    lift $ ?challenges.remove credentials.challenge
    ?users.store user
    ?publicKeys.store
        PublicKey
            { id = credentials.id
            , userId = user.id
            , algorithm = credentials.publicKeyAlgorithm
            , bytes = credentials.publicKey
            }
    pure user
