module Reacthome.Auth.Domain.RefreshTokens where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.RefreshToken

data RefreshTokens = RefreshTokens
    { findByToken :: Challenge -> MaybeT IO RefreshToken
    , store :: RefreshToken -> ExceptT String IO ()
    , remove :: RefreshToken -> IO ()
    }
