module Reacthome.Relay.Options where

data RelayOptions = RelayOptions
    { inBound :: !Int
    , chunkSize :: !Int
    }

defaultOptions :: RelayOptions
defaultOptions =
    RelayOptions
        { inBound = 4
        , chunkSize = 64
        }
