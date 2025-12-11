module Rest.Method where

import Network.HTTP.Types
import Rest
import Rest.Status

type Handler m =
    (?request :: Request) =>
    (Monad m) =>
    m Response ->
    m Response

get :: Handler m
get = match methodGet

post :: Handler m
post = match methodPost

match :: Method -> Handler m
match method controller =
    if ?request.method == method
        then controller
        else notAllowed method
