module Service.Challenge where

import Crypto.Random
import Data.ByteString
import Data.Hashable

newtype Challenge = Challenge
    { value :: ByteString
    }
    deriving (Show)
    deriving newtype (Eq, Hashable)

data ChallengeSet = ChallengeSet
    { has :: Challenge -> IO Bool
    , put :: Challenge -> IO ()
    , remove :: Challenge -> IO ()
    }

mkChallenge :: ByteString -> Challenge
mkChallenge = Challenge

mkRandomChallenge :: Int -> IO Challenge
mkRandomChallenge size = mkChallenge <$> getRandomBytes size
