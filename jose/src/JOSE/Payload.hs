module JOSE.Payload where

import Data.Aeson
import Data.Text
import Data.Time.Clock.POSIX
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import Prelude hiding (exp)

data Payload = Payload
    { jti :: UUID
    , iss :: Text
    , sub :: UUID
    , exp :: Int
    , iat :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

makePayload :: UUID -> Text -> UUID -> Int -> Int -> Payload
makePayload = Payload

newPayload :: Text -> UUID -> Int -> IO Payload
newPayload iss sub ttl = do
    jti <- nextRandom
    iat <- round <$> getPOSIXTime
    let exp = ttl + iat
    pure $
        Payload
            { jti
            , iss
            , sub
            , exp
            , iat
            }
