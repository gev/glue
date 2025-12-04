module Reacthome.Relay.Options where

data RelayOptions = RelayOptions
    { inBound :: !Int
    , chunkSize :: !Int
    }

defaultRelayOptions :: RelayOptions
defaultRelayOptions =
    RelayOptions
        { inBound = 1
        , chunkSize = 64
        }
