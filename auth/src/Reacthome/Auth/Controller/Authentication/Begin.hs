module Reacthome.Auth.Controller.Authentication.Begin where

import Control.Error.Util (exceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.Authentication.Options
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRequestOptions
import Reacthome.Auth.Domain.Authentication.Begin
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Authentication.Begin
import Rest
import Rest.Media
import Rest.Status (badRequest)

beginAuthentication ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authUsers :: AuthUsers
    , ?users :: Users
    , ?userPublicKeys :: PublicKeys
    ) =>
    IO Response
beginAuthentication = exceptT badRequest toJSON do
    options <- except =<< lift (fromJSON @AuthenticationOptions ?request)
    login <- except (makeUserLogin options.login)
    preAuthentication <- except =<< lift (runBeginAuthentication BeginAuthentication{..})
    pure (makePublicKeyCredentialRequestOptions preAuthentication)
