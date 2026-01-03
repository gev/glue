module Spec.Device (
  PortType (..),
  PortSpec (..),
  DeviceSpec (..),
) where

import Data.Text (Text)

data PortType
  = Relay
  | Dimmer
  | AnalogOutput
  | DigitalInput
  | Temperature
  deriving (Show, Eq, Ord)

data PortSpec = PortSpec
  { portType :: PortType
  , count :: Int
  }
  deriving (Show)

data DeviceSpec = DeviceSpec
  { deviceType :: Text
  , name :: Text
  , description :: Text
  , ports :: [PortSpec]
  , power :: Text
  , image :: Text
  , maxBrightness :: Double
  , fadeTime :: Double
  }
  deriving (Show)
