module Reacthome.Assist.Environment where

import Data.ByteString
import Network.Socket

data Environment = Environment
    { gate :: GateConfig
    , queueSize :: Int
    }

data GateConfig = GateConfig
    { host :: HostName
    , port :: PortNumber
    , protocol :: ByteString
    }
