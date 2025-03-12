module Reacthome.Auth.Repository.InMemory.RefreshTokens where

import Control.Concurrent
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Reacthome.Auth.Service.RefreshTokens
import Util.MVar
import Prelude hiding (lookup)

makeRefreshTokens :: (?environment :: Environment) => IO RefreshTokens
makeRefreshTokens = do
    map' <- newMVar empty
    let
        register payload = do
            challenge <- makeRandomChallenge
            runModify map' $ insert challenge payload
            pure challenge

        findBy = MaybeT . runRead map' . lookup

        remove = runModify map' . delete

    pure
        RefreshTokens
            { register
            , findBy
            , remove
            }
