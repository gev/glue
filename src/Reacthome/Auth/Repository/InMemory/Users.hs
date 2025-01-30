module Reacthome.Auth.Repository.InMemory.Users where

import Control.Concurrent
import Data.HashMap.Strict
import Data.UUID
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Util.MVar
import Prelude hiding (lookup)

mkUsers :: IO Users
mkUsers = do
    map' <- newMVar (empty, empty)
    let
        getById uid =
            runRead
                map'
                \(byId, _) ->
                    case lookup uid byId of
                        (Just user) -> Right user
                        _ -> Left $ "User not found by id " <> toString uid.value

        getByLogin login =
            runRead
                map'
                \(_, byLogin) ->
                    case lookup login byLogin of
                        (Just user) -> Right user
                        _ -> Left $ "User not found by login " <> show login.value

        store user = do
            modifyMVar
                map'
                \(byId, byLogin) ->
                    pure case lookup user.login byLogin of
                        (Just user') ->
                            if user.id == user'.id
                                then
                                    ( (insert user.id user byId, insert user.login user byLogin)
                                    , Right ()
                                    )
                                else
                                    ( (byId, byLogin)
                                    , Left $ "User with login " <> show user.login.value <> " already exists"
                                    )
                        _ ->
                            ( (insert user.id user byId, insert user.login user byLogin)
                            , Right ()
                            )

        remove user =
            runModify
                map'
                \(byId, byLogin) ->
                    (delete user.id byId, delete user.login byLogin)

    pure
        Users
            { getById
            , getByLogin
            , store
            , remove
            }
