module WebSockets.Error where

import Control.Exception (Exception (toException), SomeException)

data WebSocketError
    = SendError SomeException
    | ReceiveError SomeException
    | CloseError SomeException
    | HandshakeError SomeException
    deriving (Show)

instance Exception WebSocketError

joinExceptions ::
    (Exception e1, Exception e2) =>
    (SomeException -> WebSocketError) ->
    Either e1 (Either e2 b) ->
    Either WebSocketError b
joinExceptions err exc = do
    case exc of
        Left e1 -> Left . err . toException $ e1
        Right r1 -> case r1 of
            Left e2 -> Left . err . toException $ e2
            Right r2 -> Right r2
