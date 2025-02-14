module Reacthome.Auth.Domain.Oauth2 where

import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.Text

data AuthorizationFlow
    = CredentialGrant
    | AuthorizationCodeGrant
        { scope :: ByteString
        , state :: ByteString
        , redirect_uri :: ByteString
        , client_id :: Text
        }
    deriving stock (Show)

type AuthorizationFlowId = ByteString

data AuthorizationFlows = AuthorizationFlows
    { start :: AuthorizationFlowId -> IO AuthorizationFlow
    , findBy :: AuthorizationFlowId -> MaybeT IO AuthorizationFlow
    , remove :: AuthorizationFlowId -> IO ()
    }
