module Web.Rest.Method where

import Network.HTTP.Types
import Web.Rest
import Web.Rest.Status

type Handler a c =
    (?rest :: Rest) =>
    (Applicative a) =>
    a Response ->
    a Response

get :: Handler a c
get = match methodGet

post :: Handler a c
post = match methodPost

match :: Method -> Handler a c
match method controller =
    if ?rest.requestMethod == method
        then controller
        else notAllowed method
