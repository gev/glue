module Web.Rest.Method where

import Network.HTTP.Types
import Network.Wai
import Web.Rest.Status

type Rest a c =
    (Applicative a) =>
    (c -> Request -> a Response) ->
    c ->
    Request ->
    a Response

get :: Rest a c
get = ifMethod methodGet

post :: Rest a c
post = ifMethod methodPost

ifMethod :: Method -> Rest a c
ifMethod method run controller request =
    if request.requestMethod == method
        then run controller request
        else notAllowed method
