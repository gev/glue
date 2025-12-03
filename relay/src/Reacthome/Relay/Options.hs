module Reacthome.Relay.Options where

data RelayOptions = RelayOptions
    { inBound :: !Int
    , chunkSize :: !Int
    }

defaultOptions :: RelayOptions
defaultOptions = RelayOptions
            { inBound = 64
            , chunkSize = 64
            }