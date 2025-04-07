module Reacthome.Auth.Domain.RefreshToken where

import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Environment

data RefreshToken = RefreshToken
    { userId :: UserId
    , token :: Challenge
    }
    deriving stock (Show)

makeRefreshToken ::
    UserId ->
    Challenge ->
    RefreshToken
makeRefreshToken = RefreshToken

makeRandomRefreshToken ::
    (?environment :: Environment) =>
    UserId ->
    IO RefreshToken
makeRandomRefreshToken userId = makeRefreshToken userId <$> makeRandomChallenge
