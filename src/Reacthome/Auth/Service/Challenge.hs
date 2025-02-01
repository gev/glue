module Reacthome.Auth.Service.Challenge where

import Crypto.Random
import Data.ByteString
import Data.Hashable

newtype Challenge = Challenge
    { value :: ByteString
    }
    deriving stock (Show)
    deriving newtype (Eq, Hashable)

mkChallenge :: ByteString -> Challenge
mkChallenge = Challenge

mkRandomChallenge :: Int -> IO Challenge
mkRandomChallenge size = mkChallenge <$> getRandomBytes size
