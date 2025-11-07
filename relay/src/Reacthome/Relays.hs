module Reacthome.Relays where

import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forever, when)
import Data.ByteString.Lazy (length, splitAt)
import Data.Foldable (for_)
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
import Reacthome.Relay.Error
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
                    relay <- acceptConnection pending uid
                    run uid relay
                do
                    logError . ConnectionError uid

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
                            catch @RelayError
                                do
                                    catch @SomeException
                                        do
                                            message <- receiveData relay.connection
                                            handle uid message
                                        do
                                            throwIO . ReceiveError uid
                                \e -> do
                                    logError e
                                    close uid relay
                    do
                        close uid relay

        handle from message = do
            let messageLength = length message

            when (messageLength < 17) $ do
                throwIO
                    InvalidMessageLength
                        { messageLength = fromIntegral messageLength
                        , minimumLength = 17
                        }

            let (to, content) = splitAt 16 message
            maybe
                do
                    throwIO $ InvalidUUID to
                do
                    resend content from
                do
                    fromByteString to

        resend content from to = do
            relays <- repository.get to

            when (null relays) do
                throwIO $ NoRelaysFound to

            let message = toByteString from <> content
            for_ relays \relay ->
                catch @SomeException
                    (sendBinaryData relay.connection message)
                    \e -> do
                        logError $ SendError to e
                        close to relay

        close uid relay = do
            catch @SomeException
                do
                    sendClose relay.connection $ encodeUtf8 "Close Relay"
                do
                    logError . CloseError uid
            repository.remove uid relay

    pure Relays{..}
