module Reacthome.Auth.Domain.PublicKeys where

import Reacthome.Auth.Domain.PublicKey

data PublicKeys = PublicKeys
    { getAll :: IO (Either String [PublicKey])
    , store :: PublicKey -> IO (Either String ())
    , cleanUp :: Int -> IO (Either String ())
    }
