module JOSE.PublicKey where

import Control.Monad.Trans.Maybe
import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString
import Data.ByteString.Base64.URL
import Data.Text.Encoding
import Data.UUID
import JOSE.JWK

data PublicKey = PublicKey
    { kid :: UUID
    , publicKey :: Ed.PublicKey
    }

newtype PublicKeys m = PublicKeys
    { findBy :: UUID -> MaybeT m PublicKey
    }

makePublicKey :: UUID -> ByteString -> Either String PublicKey
makePublicKey kid bs = do
    case Ed.publicKey bs of
        CryptoFailed err -> Left $ show err
        CryptoPassed publicKey -> Right $ PublicKey kid publicKey

fromJWK :: JWK -> Either String PublicKey
fromJWK jwk =
    makePublicKey jwk.kid
        =<< decodeUnpadded (encodeUtf8 jwk.x)
