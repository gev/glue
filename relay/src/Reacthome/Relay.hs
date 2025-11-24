module Reacthome.Relay where

import Data.ByteString qualified as S
import Data.ByteString.Lazy qualified as L

type Uid = S.ByteString
type StrictRaw = S.ByteString
type LazyRaw = L.ByteString
