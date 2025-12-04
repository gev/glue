module Reacthome.Relay.Options where

data RelayOptions = RelayOptions
    { inBound :: !Int
    , chunkSize :: !Int
    }

defaultRelayOptions :: RelayOptions
defaultRelayOptions =
    RelayOptions
        { inBound = 64
        , chunkSize = 64
        }
