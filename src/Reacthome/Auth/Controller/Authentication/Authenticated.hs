module Reacthome.Auth.Controller.Authentication.Authenticated where

import Data.Aeson (ToJSON)
import Data.ByteString.Base64.URL
import Data.Text
import Data.Text.Encoding
import GHC.Generics
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialRpEntity
import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialUserEntity
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.Challenge

data Authenticated
    = Code
        { redirect_uri :: Text
        }
    | Credentials
        { rp :: PublicKeyCredentialRpEntity
        , user :: PublicKeyCredentialUserEntity
        }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

makeAuthenticated ::
    (?environment :: Environment) =>
    AuthFlow ->
    User ->
    IO Authenticated
makeAuthenticated flow user =
    case flow of
        AuthCodeGrant scope state redirect_uri client_id ->
            makeCode scope state redirect_uri client_id
        CredentialGrant ->
            pure $ makeCredentials user

makeCode ::
    (?environment :: Environment) =>
    Maybe Scope ->
    State ->
    RedirectUri ->
    ClientId ->
    IO Authenticated
makeCode scope state redirect_uri client_id = do
    code <- makeRandomChallenge ?environment.authCodeTTL
    pure
        Code
            { redirect_uri =
                decodeUtf8 $
                    redirect_uri
                        <> "?code="
                        <> encode code.value
                        <> maybe mempty ("&scope=" <>) scope
                        <> "&state="
                        <> state
                        <> "&client_id="
                        <> client_id
            }

makeCredentials ::
    (?environment :: Environment) =>
    User ->
    Authenticated
makeCredentials user =
    Credentials
        { rp = makePublicKeyCredentialRpEntity
        , user = makePublicKeyCredentialUserEntity user
        }
