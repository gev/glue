module Web.WebSockets.Options where

data WebSocketOptions = WebSocketOptions
    { bound :: !Int
    , chunkSize :: !Int
    }

defaultWebSocketOptionsOptions :: WebSocketOptions
defaultWebSocketOptionsOptions =
    WebSocketOptions
        { bound = 1
        , chunkSize = 64
        }
