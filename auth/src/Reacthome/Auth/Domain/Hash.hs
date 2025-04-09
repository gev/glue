module Reacthome.Auth.Domain.Hash where

import Crypto.Hash
import Data.ByteArray
import Data.ByteString
import Data.Hashable

newtype Hash = Hash
    { value :: ByteString
    }
    deriving stock (Show, Eq)
    deriving newtype (Hashable)

makeHash :: ByteString -> Hash
makeHash = Hash . convert . hashWith SHA256
