module Web.Rest.Method where

import Network.HTTP.Types
import Network.Wai
import Web.Rest
import Web.Rest.Status

type Handler a c =
    (?rest :: Rest) =>
    (Applicative a) =>
    (c -> a Response) ->
    c ->
    a Response

get :: Handler a c
get = match methodGet

post :: Handler a c
post = match methodPost

match :: Method -> Handler a c
match method run controller =
    if ?rest.requestMethod == method
        then run controller
        else notAllowed method
