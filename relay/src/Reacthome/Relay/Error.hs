module Reacthome.Relay.Error where

import Control.Exception (Exception)
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.UUID (UUID)
import Debug.Trace (traceIO)
import Network.WebSockets (ConnectionException, HandshakeException)
import Prelude hiding (error)

data RelayError
    = InvalidUUID ByteString
    | NoPeersFound UUID
    | SendError UUID ConnectionException
    | ReceiveError UUID ConnectionException
    | CloseError UUID ConnectionException
    | HandshakeError UUID HandshakeException
    | ConnectionError UUID ConnectionException
    | InvalidMessageLength
        { messageLength :: Int64
        , minimumLength :: Int64
        }
    deriving (Show)

instance Exception RelayError

logError :: RelayError -> IO ()
logError err =
    traceIO $
        "[ERROR] " <> case err of
            InvalidMessageLength{..} ->
                "Invalid message length: got "
                    <> show messageLength
                    <> ", expected at least "
                    <> show minimumLength
            InvalidUUID bytes ->
                "Invalid UUID in message: " <> show bytes
            NoPeersFound peer ->
                "No relays found for UUID: " <> show peer
            ReceiveError peer e ->
                "Failed to receive message from " <> show peer <> ": " <> show e
            SendError peer e ->
                "Failed to send message to " <> show peer <> ": " <> show e
            CloseError peer e ->
                "Failed to close connection " <> show peer <> ": " <> show e
            HandshakeError peer e ->
                "Handshake error for " <> show peer <> ": " <> show e
            ConnectionError peer e ->
                "Connection error for " <> show peer <> ": " <> show e
