module Reacthome.Auth.Domain.RefreshTokens where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Hash (Hash)
import Reacthome.Auth.Domain.RefreshToken

data RefreshTokens = RefreshTokens
    { findByHash :: Hash -> MaybeT IO RefreshToken
    , store :: RefreshToken -> ExceptT String IO ()
    , remove :: RefreshToken -> IO ()
    }
