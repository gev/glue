module Reacthome.Auth.Repository.RefreshTokens.InMemory where

import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Reacthome.Auth.Domain.RefreshToken
import Reacthome.Auth.Domain.RefreshTokens
import Reacthome.Auth.Environment
import Util.MVar
import Prelude hiding (lookup)

makeRefreshTokens :: (?environment :: Environment) => IO RefreshTokens
makeRefreshTokens = do
    map' <- newMVar empty
    let
        findByHash = MaybeT . runRead map' . lookup
        store token = lift $ runModify map' $ insert token.hash token
        remove token = runModify map' $ delete token.hash
    pure
        RefreshTokens
            { findByHash
            , store
            , remove
            }
