module Reacthome.Relay.Error where

import Control.Exception (Exception, SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Data.UUID (UUID)
import Debug.Trace (traceIO)
import Prelude hiding (error)

data RelayError
    = InvalidUUID ByteString
    | NoPeersFound UUID
    | SendError UUID SomeException
    | ReceiveError UUID SomeException
    | CloseError UUID SomeException
    | ConnectionError
        { peer :: UUID
        , error :: SomeException
        }
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
            NoPeersFound uid ->
                "No relays found for UUID: " <> show uid
            ReceiveError uid e ->
                "Failed to receive message from " <> show uid <> ": " <> show e
            SendError uid e ->
                "Failed to send message to " <> show uid <> ": " <> show e
            CloseError uid e ->
                "Failed to close connection " <> show uid <> ": " <> show e
            ConnectionError{..} ->
                "Connection error for " <> show peer <> ": " <> show error
