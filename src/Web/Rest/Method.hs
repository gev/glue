module Web.Rest.Method where

import Network.HTTP.Types
import Network.Wai
import Web.Rest
import Web.Rest.Status

type Rest' a c =
    (?rest :: Rest) =>
    (Applicative a) =>
    (c -> a Response) ->
    c ->
    a Response

get :: Rest' a c
get = ifMethod methodGet

post :: Rest' a c
post = ifMethod methodPost

ifMethod :: (?rest :: Rest) => Method -> Rest' a c
ifMethod method run controller =
    if ?rest.requestMethod == method
        then run controller
        else notAllowed method
