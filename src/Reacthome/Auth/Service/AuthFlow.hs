module Reacthome.Auth.Service.AuthFlow where

import Data.ByteString

type Scope = ByteString
type State = ByteString
type RedirectUri = ByteString
type ClientId = ByteString

data AuthFlow
    = CredentialGrant
    | AuthCodeGrant
        { scope :: Maybe Scope
        , state :: State
        , redirect_uri :: RedirectUri
        , client_id :: ClientId
        }
    deriving stock (Show)
