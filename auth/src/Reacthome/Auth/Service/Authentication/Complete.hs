module Reacthome.Auth.Service.Authentication.Complete where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Authentication.Complete
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKey.Id
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers

runCompleteAuthentication ::
  ( ?environment :: Environment
  , ?authUsers :: AuthUsers
  , ?users :: Users
  , ?publicKeys :: PublicKeys
  ) =>
  CompleteAuthentication ->
  ExceptT String IO User
runCompleteAuthentication request = do
  user <-
    maybeToExceptT
      ("Invalid challenge " <> show request.challenge.value)
      $ ?authUsers.findBy request.challenge
  lift $ ?authUsers.remove request.challenge
  publicKey <-
    maybeToExceptT ("Public key with id " <> show request.id.value <> " not found") $
      ?publicKeys.findById request.id
  isValidSignature <- verifySignature publicKey request.message request.signature
  if isValidSignature
    then pure user
    else
      throwE $
        "Cant verify signature for user id: `"
          <> show user.id
          <> "` by public key id `"
          <> show publicKey.id
          <> "` "
