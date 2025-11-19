module Reacthome.Relay.Error where

import Control.Exception (Exception)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (traceIO)
import Web.WebSockets.Error (WebSocketError)
import Prelude hiding (error)

data RelayError
    = InvalidUUID ByteString
    | NoPeersFound ByteString
    | WebSocketError ByteString WebSocketError
    | InvalidMessageLength
        { messageLength :: Int
        , minimumLength :: Int
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
            WebSocketError peer e ->
                "WebSocket error, peer " <> show peer <> ": " <> show e
