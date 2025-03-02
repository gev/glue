module JOSE.KeyPair where

import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteArray.Encoding
import Data.ByteString
import Data.Text.Encoding
import Data.UUID
import Data.UUID.V4
import JOSE.JWK

data KeyPair = KeyPair
    { kid :: UUID
    , secretKey :: Ed.SecretKey
    , publicKey :: Ed.PublicKey
    }
    deriving stock (Show)

makeKeyPair :: UUID -> ByteString -> Either String KeyPair
makeKeyPair kid bs = do
    case Ed.secretKey bs of
        CryptoFailed err -> Left $ show err
        CryptoPassed secretKey -> Right $ fromSecret kid secretKey

generateKeyPair :: IO KeyPair
generateKeyPair = do
    kid <- nextRandom
    fromSecret kid <$> Ed.generateSecretKey

fromSecret :: UUID -> Ed.SecretKey -> KeyPair
fromSecret kid secretKey =
    KeyPair
        { kid
        , secretKey
        , publicKey = Ed.toPublic secretKey
        }

toJWK :: KeyPair -> JWK
toJWK kp = do
    JWK
        { kty = OKP
        , crv = Ed25519
        , x = decodeUtf8 $ convertToBase Base64URLUnpadded kp.publicKey
        , kid = kp.kid
        }
