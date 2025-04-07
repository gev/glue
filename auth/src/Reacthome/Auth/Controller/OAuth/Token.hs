module Reacthome.Auth.Controller.OAuth.Token where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import GHC.Generics
import JOSE.KeyPair
import JOSE.Sign qualified as JOSE
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.RefreshToken
import Reacthome.Auth.Domain.RefreshTokens
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Environment
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
    , ?refreshTokens :: RefreshTokens
    , ?keyPair :: KeyPair
    ) =>
    UserId ->
    ExceptT String IO Token
generateToken uid = do
    access_token <-
        lift $
            JOSE.generateToken
                ?keyPair
                ?environment.domain
                ?environment.accessTokenTTL
                uid.value
    refresh <- lift $ makeRandomRefreshToken uid
    ?refreshTokens.store refresh
    pure
        Token
            { access_token = decodeUtf8 access_token
            , token_type = Bearer
            , expires_in = ?environment.accessTokenTTL
            , refresh_token = toBase64 refresh.token.value
            }
