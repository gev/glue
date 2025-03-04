module Reacthome.Auth.Controller.OAuth.Token where

import Data.Aeson
import Data.Text
import Data.Text.Encoding
import GHC.Generics
import JOSE.KeyPair
import JOSE.Sign qualified as JOSE
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthUsers
import Reacthome.Auth.Service.Challenge
import Util.Base64.URL

data TokenType = Bearer
    deriving stock (Generic, Show)

instance ToJSON TokenType where
    toJSON =
        genericToJSON
            defaultOptions
                { tagSingleConstructors = True
                }

{-
    Should add a scope
-}
data Token = Token
    { access_token :: Text
    , token_type :: TokenType
    , expires_in :: Int
    , refresh_token :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

generateToken ::
    ( ?environment :: Environment
    , ?authUsers :: AuthUsers
    , ?keyPair :: KeyPair
    ) =>
    User ->
    IO Token
generateToken user = do
    let expires_in = 60
    access_token <- JOSE.generateToken ?keyPair ?environment.domain user.id.value expires_in
    refresh_token <- ?authUsers.register user
    pure
        Token
            { access_token = decodeUtf8 access_token
            , token_type = Bearer
            , expires_in
            , refresh_token = toBase64 refresh_token.value
            }
