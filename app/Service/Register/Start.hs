module Service.Register.Start where

import Control.Concurrent
import Control.Monad
import Crypto.Random
import Data.ByteString
import Environment
import Service.Challenge
import Service.WebAuthn.PublicKeyCredentialCreationOptions
import Service.WebAuthn.PublicKeyCredentialRpEntity
import Service.WebAuthn.PublicKeyCredentialUserEntity
import Service.WebAuthn.RegisterOptions

startRegister ::
    ( ?environment :: Environment
    , ?challenges :: ChallengeSet
    ) =>
    RegisterOptions ->
    IO (Maybe PublicKeyCredentialCreationOptions)
startRegister =
    mkCreationOptions . validateRegisterOptions

mkCreationOptions ::
    ( ?environment :: Environment
    , ?challenges :: ChallengeSet
    ) =>
    Maybe ValidRegisterOptions ->
    IO (Maybe PublicKeyCredentialCreationOptions)
mkCreationOptions Nothing = pure Nothing
mkCreationOptions (Just options) = do
    uid <- getRandomBytes 20
    challenge <- mkAutoRemovedChallenge
    let user = mkUserEntity uid options
    pure . Just $
        mkPublicKeyCredentialCreationOptions
            mkRpEntity
            user
            challenge.value
            ?environment.timeout

mkAutoRemovedChallenge ::
    ( ?environment :: Environment
    , ?challenges :: ChallengeSet
    ) =>
    IO Challenge
mkAutoRemovedChallenge = do
    challenge <- mkRandomChallenge 20
    ?challenges.put challenge
    void $ autoRemovedChallenge challenge
    pure challenge

autoRemovedChallenge ::
    ( ?environment :: Environment
    , ?challenges :: ChallengeSet
    ) =>
    Challenge ->
    IO ThreadId
autoRemovedChallenge challenge = forkIO do
    threadDelay $ 1_000 * ?environment.timeout
    ?challenges.remove challenge

mkUserEntity ::
    ByteString ->
    ValidRegisterOptions ->
    PublicKeyCredentialUserEntity
mkUserEntity uid valid =
    mkPublicKeyCredentialUserEntity
        uid
        valid.options.name
        valid.options.displayName

mkRpEntity ::
    (?environment :: Environment) =>
    PublicKeyCredentialRpEntity
mkRpEntity =
    PublicKeyCredentialRpEntity
        { id = Just ?environment.domain
        , name = ?environment.name
        }
