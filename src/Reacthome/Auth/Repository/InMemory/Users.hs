module Reacthome.Auth.Repository.InMemory.Users where

import Control.Concurrent
import Data.HashMap.Strict
import Data.Maybe (isJust)
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Util.MVar
import Prelude hiding (lookup)

mkUsers :: IO Users
mkUsers = do
    map' <- newMVar (empty, empty)
    let
        findById uid =
            runRead
                map'
                \(byId, _) -> lookup uid byId

        findByLogin login =
            runRead
                map'
                \(_, byLogin) -> lookup login byLogin

        has login =
            runRead
                map'
                \(_, byLogin) -> isJust $ lookup login byLogin

        store user =
            modifyMVar
                map'
                \(byId, byLogin) ->
                    pure case lookup user.login byLogin of
                        (Just user') ->
                            if user.id == user'.id
                                then
                                    (
                                        ( insert user.id user byId
                                        , insert user.login user (delete user'.login byLogin)
                                        )
                                    , Right ()
                                    )
                                else
                                    (
                                        ( byId
                                        , byLogin
                                        )
                                    , Left $ "User with login " <> show user.login.value <> " already exists"
                                    )
                        _ ->
                            (
                                ( insert user.id user byId
                                , insert user.login user byLogin
                                )
                            , Right ()
                            )

        remove user =
            runModify
                map'
                \(byId, byLogin) ->
                    (delete user.id byId, delete user.login byLogin)

    pure
        Users
            { findById
            , findByLogin
            , has
            , store
            , remove
            }
