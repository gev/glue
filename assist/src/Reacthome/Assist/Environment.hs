module Reacthome.Assist.Environment where

import Data.ByteString (ByteString)
import Network.Socket

newtype Environment = Environment
    { gate :: GateConfig
    }

data GateConfig = GateConfig
    { host :: HostName
    , port :: PortNumber
    , protocol :: ByteString
    }
