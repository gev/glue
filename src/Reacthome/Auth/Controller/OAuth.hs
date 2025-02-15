module Reacthome.Auth.Controller.OAuth where

import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.AuthFlows
import Web.Rest
import Web.Rest.Status

oauth ::
    ( ?request :: Request
    , ?authFlows :: AuthFlows
    , Applicative a
    ) =>
    a Response
oauth = do
    let responseType = ?request.query "response_type"
    case responseType of
        Just "code" -> do
            let flow = do
                    let scope = ?request.query "scope"
                    state <- ?request.query "state"
                    redirect_uri <- ?request.query "redirect_uri"
                    client_id <- ?request.query "client_id"
                    pure
                        AuthCodeGrant
                            { scope
                            , state
                            , redirect_uri
                            , client_id
                            }
            case flow of
                Just _ -> do
                    redirect mempty "/authentication"
                _ -> badRequest "Invalid OAuth2 Authorization Code Grant flow parameters"
        _ -> badRequest "Unknown OAuth2 authorization glow"
