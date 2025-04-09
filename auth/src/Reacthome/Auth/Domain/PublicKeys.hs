module Reacthome.Auth.Domain.PublicKeys where

import Reacthome.Auth.Domain.PublicKey

data PublicKeys = PublicKeys
    { get :: IO [PublicKey]
    , store :: PublicKey -> IO ()
    , cleanUp :: Int -> IO ()
    }
