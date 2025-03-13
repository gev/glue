module Reacthome.Auth.View.Screen.Authentication where

import Lucid

authentication :: Html ()
authentication =
    doctypehtml_ do
        head_ do
            title_ "Reacthome. Authentication"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "description", content_ "Reacthome Auth Service"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_ [rel_ "icon", type_ "image/png", href_ "/icon.png"]
            link_ [rel_ "stylesheet", href_ "/styles.css"]
            script_ [src_ "/auth.js"] (mempty :: Html ())
        body_ [onload_ "init(authenticate)"] do
            div_ do
                img_ [src_ "/icon.png", alt_ "Reacthome logo"]
            div_ [class_ "row"] do
                h2_ "Authentication"
                a_ [href_ "/registration"] "Sign up"
            div_ do
                form_ [id_ "form"] do
                    input_ [name_ "login", type_ "text", placeholder_ "Login", autocomplete_ "on", autofocus_]
                    button_ [type_ "submit"] "Sign in"
            div_ [id_ "debug"] mempty
