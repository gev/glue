module JOSE.JWK where

import Data.Aeson
import Data.ByteArray.Encoding
import Data.ByteString.Base64.URL
import Data.Text
import Data.Text.Encoding
import Data.UUID
import GHC.Generics
import JOSE.PublicKey
import JOSE.Util

data JWK = JWK
    { kty :: Kty
    , crv :: Crv
    , x :: Text
    , kid :: UUID
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data Kty = OKP
    deriving stock (Generic, Show)

instance FromJSON Kty where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Kty where
    toJSON = genericToJSON aesonOptions

data Crv = Ed25519
    deriving stock (Generic, Show)

instance FromJSON Crv where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Crv where
    toJSON = genericToJSON aesonOptions

fromJWK :: JWK -> Either String PublicKey
fromJWK jwk =
    makePublicKey jwk.kid
        =<< decodeUnpadded (encodeUtf8 jwk.x)

toJWK :: PublicKey -> JWK
toJWK pk = do
    JWK
        { kty = OKP
        , crv = Ed25519
        , x = decodeUtf8 $ convertToBase Base64URLUnpadded pk.publicKey
        , kid = pk.kid
        }
