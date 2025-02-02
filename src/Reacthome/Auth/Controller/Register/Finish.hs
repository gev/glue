module Reacthome.Auth.Controller.Register.Finish where

import Control.Monad.Trans.Except
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredential
import Reacthome.Auth.Controller.WebAuthn.RegisteredOptions
import Reacthome.Auth.Domain.Credential.PublicKeys
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Register.Challenges
import Reacthome.Auth.Service.Register.Finish

finishRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    , ?publicKeys :: PublicKeys
    ) =>
    EncodedPublicKeyCredential ->
    ExceptT String IO RegisteredOptions
finishRegister = mkRegisteredOptions . decodePublicKeyCredential
