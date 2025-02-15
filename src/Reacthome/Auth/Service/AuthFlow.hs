module Reacthome.Auth.Service.AuthFlow where

import Data.ByteString
import Data.Text

type Scope = ByteString
type State = ByteString
type RedirectUri = ByteString
type ClientId = Text

data AuthFlow
    = CredentialGrant
    | AuthCodeGrant
        { scope :: Scope
        , state :: State
        , redirect_uri :: RedirectUri
        , client_id :: ClientId
        }
    deriving stock (Show)
