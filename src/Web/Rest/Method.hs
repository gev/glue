module Web.Rest.Method where

import Network.HTTP.Types
import Web.Rest
import Web.Rest.Status

type Handler a c =
    (?request :: Request) =>
    (Applicative a) =>
    a Response ->
    a Response

get :: Handler a c
get = match methodGet

post :: Handler a c
post = match methodPost

match :: Method -> Handler a c
match method controller =
    if ?request.method == method
        then controller
        else notAllowed method
