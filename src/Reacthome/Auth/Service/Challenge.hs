module Reacthome.Auth.Service.Challenge where

import Crypto.Random
import Data.ByteString
import Data.Hashable

newtype Challenge = Challenge
    { value :: ByteString
    }
    deriving stock (Show)
    deriving newtype (Eq, Hashable)

makeChallenge :: ByteString -> Challenge
makeChallenge = Challenge

makeRandomChallenge :: Int -> IO Challenge
makeRandomChallenge size = makeChallenge <$> getRandomBytes size
