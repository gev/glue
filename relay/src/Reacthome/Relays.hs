module Reacthome.Relays where

import Control.Exception (SomeException, catch, finally, try)
import Control.Monad (forever, when)
import Data.ByteString.Lazy (length, splitAt)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID, fromByteString, toByteString)
import Network.WebSockets (
    PendingConnection,
    acceptRequestWith,
    defaultAcceptRequest,
    receiveData,
    sendBinaryData,
    sendClose,
    withPingThread,
 )
import Reacthome.Relay (connection, makeRelay)
import Reacthome.Relay.Repository (add, get, makeRepository, remove)
import Prelude hiding (length, splitAt, tail)

newtype Relays = Relays
    { connect :: PendingConnection -> UUID -> IO ()
    }

makeRelays :: IO Relays
makeRelays = do
    repository <- makeRepository

    let
        connect pending uid =
            catch @SomeException
                do
                    connection <- acceptConnection pending uid
                    run uid connection
                do
                    const $ pure ()

        acceptConnection pending uid = do
            connection <- acceptRequestWith pending defaultAcceptRequest
            relay <- makeRelay connection
            repository.add uid relay
            pure relay

        run uid relay =
            withPingThread relay.connection 30 (pure ()) do
                finally
                    do
                        forever do
                            catch @SomeException
                                do
                                    message <- receiveData relay.connection
                                    handle uid message
                                do
                                    const $ close uid relay
                    do
                        close uid relay

        handle from message =
            when (length message > 16) do
                let (to, content) = splitAt 16 message
                maybe
                    do
                        pure ()
                    do
                        resend content from
                    do
                        fromByteString to

        resend content from to = do
            relays <- repository.get to
            let message = toByteString from <> content
            for_ relays \relay ->
                catch @SomeException
                    do
                        sendBinaryData relay.connection message
                    do
                        const $ close to relay

        close uid relay = do
            void $ try @SomeException do
                sendClose relay.connection $ encodeUtf8 "Close Relay"
            repository.remove uid relay

    pure Relays{..}
