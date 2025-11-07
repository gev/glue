module Reacthome.Relays where

import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forever, when)
import Data.ByteString.Lazy (length, splitAt)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID, fromByteString, toByteString)
import Network.WebSockets (
    PendingConnection,
    acceptRequestWith,
    defaultAcceptRequest,
    defaultPingPongOptions,
    receiveData,
    sendBinaryData,
    sendClose,
    withPingPong,
 )
import Reacthome.Relay (connection, makeRelay)
import Reacthome.Relay.Error (RelayError (..), logError)
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
                    connection <- acceptRequestWith pending defaultAcceptRequest
                    withPingPong defaultPingPongOptions connection $ run uid
                do
                    logError . ConnectionError uid

        run uid connection = do
            relay <- makeRelay connection
            repository.add uid relay
            finally
                do
                    forever $ loop uid relay
                do
                    close uid relay

        loop uid relay =
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

        handle from message = do
            (to, content) <- parse message
            resend content from to

        parse message = do
            validate message
            let (to, content) = splitAt headerLength message
            maybe
                do
                    throwIO $ InvalidUUID to
                do
                    pure . (,content)
                do
                    fromByteString to

        validate message = do
            let messageLength = length message
            when (messageLength < minimumMessageLength) $ do
                throwIO $
                    InvalidMessageLength
                        messageLength
                        minimumMessageLength

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

headerLength :: Int64
headerLength = 16

minimumMessageLength :: Int64
minimumMessageLength = headerLength + 1
