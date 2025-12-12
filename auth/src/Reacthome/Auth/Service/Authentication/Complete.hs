module Reacthome.Auth.Service.Authentication.Complete where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Reacthome.Auth.Domain.Authentication.Complete
import Reacthome.Auth.Domain.Credential.PublicKey
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers

runCompleteAuthentication ::
  ( ?environment :: Environment
  , ?authUsers :: AuthUsers
  , ?users :: Users
  , ?userPublicKeys :: PublicKeys
  ) =>
  CompleteAuthentication ->
  IO (Either String User)
runCompleteAuthentication request = runExceptT do
  user <- except =<< lift (?authUsers.findBy request.challenge)
  lift $ ?authUsers.remove request.challenge
  publicKey <- except =<< lift (?userPublicKeys.findById request.id)
  isValidSignature <- except (verifySignature publicKey request.message request.signature)
  if isValidSignature
    then pure user
    else
      throwE $
        "Cant verify signature for user id: `"
          <> show user.id
          <> "` by public key id `"
          <> show publicKey.id
          <> "` "
