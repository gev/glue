module Reacthome.Auth.Service.Registration.Complete where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
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
    IO (Either String User)
runCompleteRegistration credentials = runExceptT do
    user <- except =<< lift (?authUsers.findBy credentials.challenge)
    lift (?authUsers.remove credentials.challenge)
    except =<< lift (?users.store user)
    bytes <-
        except $
            decodePublicKey
                credentials.publicKeyAlgorithm
                credentials.publicKey
    except
        =<< lift
            ( ?userPublicKeys.store
                PublicKey
                    { id = credentials.id
                    , userId = user.id
                    , algorithm = credentials.publicKeyAlgorithm
                    , bytes
                    }
            )
    pure user
