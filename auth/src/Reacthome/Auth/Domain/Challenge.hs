module Reacthome.Auth.Domain.Challenge where

import Crypto.Random
import Data.ByteString
import Data.Hashable
import Reacthome.Auth.Environment

newtype Challenge = Challenge
    { value :: ByteString
    }
    deriving stock (Show)
    deriving newtype (Eq, Hashable)

makeChallenge :: ByteString -> Challenge
makeChallenge = Challenge

makeRandomChallenge :: (?environment :: Environment) => IO Challenge
makeRandomChallenge = makeChallenge <$> getRandomBytes ?environment.challengeSize
