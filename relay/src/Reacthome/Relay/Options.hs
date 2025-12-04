module Reacthome.Relay.Options where

data RelayOptions = RelayOptions
    { inBound :: !Int
    , chunkSize :: !Int
    , delay :: !Int
    }

defaultRelayOptions :: RelayOptions
defaultRelayOptions =
    RelayOptions
        { inBound = 2
        , chunkSize = 64
        , delay = 300
        }
