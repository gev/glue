module Reacthome.Auth.Controller.Register.Start where

import Reacthome.Auth.Controller.WebAuthn.PublicKeyCredentialCreationOptions
import Reacthome.Auth.Controller.WebAuthn.RegisterOptions

import Reacthome.Auth.Domain.Register.Start
import Reacthome.Auth.Domain.Users
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Register.Challenges
import Reacthome.Auth.Service.Register.Start

startRegister ::
    ( ?environment :: Environment
    , ?challenges :: RegisterChallenges
    , ?users :: Users
    ) =>
    RegisterOptions ->
    IO (Either String PublicKeyCredentialCreationOptions)
startRegister options =
    case mkStartRegisterUser options.login options.name of
        Left err -> pure $ Left err
        Right command -> do
            preRegisteredUser <- runStartRegister command
            pure $ mkPublicKeyCredentialCreationOptions <$> preRegisteredUser

-- startRegister ::
--     ( ?environment :: Environment
--     , ?challenges :: RegisterChallenges
--     , ?users :: Users
--     ) =>
--     RegisterOptions ->
--     Either String (IO PublicKeyCredentialCreationOptions)
-- startRegister options =
--     case mkStartRegisterUser options.login options.name of
--         Left err -> Left err
--         Right command ->
--             Right $ do
--                 preRegisteredResult <- runStartRegister command
--                 case preRegisteredResult of
--                     Left err -> fail err -- Handle the error within IO (e.g., throw an exception)
--                     Right preRegisteredUser ->
--                         pure $ mkPublicKeyCredentialCreationOptions preRegisteredUser
