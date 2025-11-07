module Reacthome.Relay.Error where

import Control.Exception (Exception, SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.UUID (UUID)
import Debug.Trace (traceIO)
import Prelude hiding (error)

data RelayError
    = InvalidMessageLength
        { messageLength :: Int
        , minimumLength :: Int
        }
    | InvalidUUID ByteString
    | NoRelaysFound UUID
    | SendError UUID SomeException
    | ReceiveError UUID SomeException
    | CloseError UUID SomeException
    | ConnectionError
        { clientId :: UUID
        , error :: SomeException
        }
    deriving (Show)

instance Exception RelayError

logError :: RelayError -> IO ()
logError err =
    traceIO $
        "[ERROR] " ++ case err of
            InvalidMessageLength{..} ->
                "Invalid message length: got "
                    ++ show messageLength
                    ++ ", expected at least "
                    ++ show minimumLength
            InvalidUUID bytes ->
                "Invalid UUID in message: " ++ show bytes
            NoRelaysFound uid ->
                "No relays found for UUID: " ++ show uid
            ReceiveError uid e ->
                "Failed to receive message from " ++ show uid ++ ": " ++ show e
            SendError uid e ->
                "Failed to send message to " ++ show uid ++ ": " ++ show e
            CloseError uid e ->
                "Failed to close connection " ++ show uid ++ ": " ++ show e
            ConnectionError{..} ->
                "Connection error for " ++ show clientId ++ ": " ++ show error
