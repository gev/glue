module Web.WebSockets.Options where

data WebSocketOptions = WebSocketOptions
    { bound :: !Int
    , chunkSize :: !Int
    , delay :: !Int
    }

defaultWebSocketOptionsOptions :: WebSocketOptions
defaultWebSocketOptionsOptions =
    WebSocketOptions
        { bound = 2
        , chunkSize = 64
        , delay = 300
        }
