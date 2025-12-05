module Rest.Method where

import Control.Monad.Trans.Except
import Network.HTTP.Types
import Rest
import Rest.Status

type Handler m =
    (?request :: Request) =>
    (Monad m) =>
    ExceptT String m Response ->
    m Response

get :: Handler m
get = match methodGet

post :: Handler m
post = match methodPost

match :: Method -> Handler m
match method controller =
    if ?request.method == method
        then either badRequest pure =<< runExceptT controller
        else notAllowed method
