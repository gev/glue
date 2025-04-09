module Reacthome.Auth.Domain.PublicKey where

import Data.ByteArray
import Data.ByteString
import Data.UUID
import JOSE.KeyPair

data PublicKey = PublicKey
    { kid :: UUID
    , bytes :: ByteString
    , timestamp :: Int
    }
    deriving stock (Show)

instance Eq PublicKey where
    a == b = a.kid == b.kid

makePublicKey :: KeyPair -> Int -> PublicKey
makePublicKey pk timestamp =
    PublicKey
        { kid = pk.kid
        , bytes = convert pk.publicKey
        , timestamp
        }
