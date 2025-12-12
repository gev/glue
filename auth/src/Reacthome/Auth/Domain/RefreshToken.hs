module Reacthome.Auth.Domain.RefreshToken where

import Data.ByteString
import Reacthome.Auth.Domain.Hash
import Reacthome.Auth.Domain.User.Id

data RefreshToken = RefreshToken
    { userId :: UserId
    , hash :: Hash
    }
    deriving stock (Show, Eq)

makeRefreshToken ::
    UserId ->
    ByteString ->
    RefreshToken
makeRefreshToken userId value =
    RefreshToken
        { userId
        , hash = makeHash value
        }
