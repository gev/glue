module Reacthome.Auth.Repository.Users.InMemory where

import Control.Concurrent
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.HashMap.Strict
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Util.MVar
import Prelude hiding (lookup)

makeUsers :: IO Users
makeUsers = do
    map' <- newMVar (empty, empty)
    let
        findById uid =
            MaybeT $ runRead
                map'
                \(byId, _) -> lookup uid byId

        findByLogin login =
            MaybeT $ runRead
                map'
                \(_, byLogin) -> lookup login byLogin

        has login =
            runRead
                map'
                \(_, byLogin) -> member login byLogin

        store user =
            ExceptT $ modifyMVar
                map'
                \value@(byId, byLogin) ->
                    pure case lookup user.login byLogin of
                        Just user' ->
                            if user.id == user'.id
                                then
                                    (
                                        ( insert user.id user byId
                                        , insert user.login user (delete user'.login byLogin)
                                        )
                                    , Right ()
                                    )
                                else
                                    ( value
                                    , Left $
                                        "User with login "
                                            <> show user.login.value
                                            <> " already exists"
                                    )
                        Nothing ->
                            (
                                ( insert user.id user byId
                                , insert user.login user byLogin
                                )
                            , Right ()
                            )

        remove user =
            runModify map' $
                bimap
                    (delete user.id)
                    (delete user.login)

    pure
        Users
            { findById
            , findByLogin
            , has
            , store
            , remove
            }
