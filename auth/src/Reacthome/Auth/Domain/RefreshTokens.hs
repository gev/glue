module Reacthome.Auth.Domain.RefreshTokens where

import Reacthome.Auth.Domain.Hash (Hash)
import Reacthome.Auth.Domain.RefreshToken

data RefreshTokens = RefreshTokens
    { findByHash :: Hash -> IO (Either String RefreshToken)
    , store :: RefreshToken -> IO (Either String ())
    , remove :: RefreshToken -> IO (Either String ())
    }
