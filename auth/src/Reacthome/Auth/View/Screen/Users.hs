{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Reacthome.Auth.View.Screen.Users where

import Data.Foldable
import Data.UUID
import Lucid
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id
import Reacthome.Auth.Domain.User.Login
import Reacthome.Auth.Domain.User.Name

users :: [User] -> Html ()
users us =
    doctypehtml_ do
        head_ do
            title_ "Reacthome. Users"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "description", content_ "Reacthome Auth Service"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_ [rel_ "icon", type_ "image/png", href_ "/icon.png"]
            link_ [rel_ "stylesheet", href_ "/styles.css"]
        body_ do
            div_ do
                img_ [src_ "/icon.png", alt_ "Reacthome logo"]
            div_ [class_ "row"] do
                h2_ "Users"
            traverse_ userRow us

userRow :: User -> Html ()
userRow u =
    div_ [class_ "row"] do
        p_ do
            small_ . toHtml $ toText u.id.value
            br_ mempty
            toHtml $ u.login.value <> " / " <> u.name.value
