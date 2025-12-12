module Reacthome.Auth.Repository.Users.InMemory where

import Control.Concurrent
import Control.Error (note)
import Data.Bifunctor
import Data.HashMap.Strict
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.Users
import Util.MVar
import Prelude hiding (lookup)

makeUsers :: IO Users
makeUsers = do
    map' <- newMVar (empty, empty)
    let
        getAll =
            Right <$> runRead map' \(byId, _) -> snd <$> toList byId

        findById uid =
            note ("User " <> show uid.value <> " not found")
                <$> runRead map' \(byId, _) -> lookup uid byId

        findByLogin login =
            note ("User " <> show login.value <> " not found")
                <$> runRead map' \(_, byLogin) -> lookup login byLogin

        has login =
            Right <$> runRead map' \(_, byLogin) -> member login byLogin

        store user =
            modifyMVar
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
            Right <$> runModify map' do
                bimap
                    (delete user.id)
                    (delete user.login)

    pure Users{..}
