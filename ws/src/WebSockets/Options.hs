module WebSockets.Options where

data WebSocketOptions = WebSocketOptions
    { bound :: !Int
    , chunkSize :: !Int
    , delay :: !Int
    }

defaultWebSocketOptions :: WebSocketOptions
defaultWebSocketOptions =
    WebSocketOptions
        { bound = 2
        , chunkSize = 64
        , delay = 300
        }
