module Reacthome.Auth.Controller.Registration.Begin where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Registration.Options
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Domain.Registration.Begin
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Registration.Begin
import Rest
import Rest.Media
import Rest.Status (badRequest)

beginRegistration ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authUsers :: AuthUsers
    , ?users :: Users
    ) =>
    IO Response
beginRegistration = exceptT badRequest toJSON do
    options <- except =<< lift (fromJSON @RegistrationOptions ?request)
    login <- except (makeUserLogin options.login)
    name <- except (makeUserName options.name)
    preRegistration <- except =<< lift (runBeginRegistration BeginRegistration{..})
    pure (makePublicKeyCredentialCreationOptions preRegistration)
