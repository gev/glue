module Reacthome.Auth.Repository.RefreshTokens.InMemory where

import Control.Concurrent
import Control.Error (note)
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
        findByHash hash = note ("Refresh token " <> show hash <> " not found") <$> runRead map' (lookup hash)
        store token = Right <$> runModify map' (insert token.hash token)
        remove token = Right <$> runModify map' (delete token.hash)
    pure RefreshTokens{..}
