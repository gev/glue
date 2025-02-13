module Reacthome.Auth.View.Screen.Registration where

import Lucid

registration :: Html ()
registration =
    doctypehtml_ do
        head_ do
            title_ "Reacthome. Registration"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "description", content_ "Reacthome Auth Service"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_ [rel_ "icon", type_ "image/png", href_ "/icon.png"]
            link_ [rel_ "stylesheet", href_ "/styles.css"]
            script_ [src_ "/auth.js"] (mempty :: Html ())
        body_ [onload_ "init(register)"] do
            div_ do
                img_ [width_ "150px", src_ "/icon.png", alt_ "Reacthome logo"]
                h2_ "Reacthome"
                h1_ "Registration"
            form_ [id_ "form"] do
                div_ do
                    input_ [name_ "login", type_ "text", placeholder_ "Login", autocomplete_ "on", autofocus_]
                div_ do
                    input_ [name_ "name", type_ "text", placeholder_ "Name", autocomplete_ "on"]
                div_ do
                    button_ [type_ "submit"] "Sign up"
                    a_ [href_ "/"] "Sign in"
            div_ [id_ "debug"] mempty
