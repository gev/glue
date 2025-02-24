module JOSE.Payload where

import Data.Aeson
import Data.Text
import Data.Time
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import Prelude hiding (exp)

data Payload = Payload
    { jti :: UUID
    , iss :: Text
    , sub :: UUID
    , exp :: UTCTime
    , iat :: UTCTime
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

makePayload :: UUID -> Text -> UUID -> UTCTime -> UTCTime -> Payload
makePayload = Payload

newPayload :: Text -> UUID -> NominalDiffTime -> IO Payload
newPayload iss sub age = do
    jti <- nextRandom
    iat <- getCurrentTime
    let exp = addUTCTime age iat
    pure
        Payload
            { jti
            , iss
            , sub
            , exp
            , iat
            }
