module JOSE.PublicKey where

import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString
import Data.UUID

data PublicKey = PublicKey
    { kid :: UUID
    , publicKey :: Ed.PublicKey
    }
    deriving stock (Show)

data PublicKeys m = PublicKeys
    { findBy :: UUID -> m (Either String PublicKey)
    , store :: [PublicKey] -> m ()
    }

makePublicKey :: UUID -> ByteString -> Either String PublicKey
makePublicKey kid bs = do
    case Ed.publicKey bs of
        CryptoFailed err -> Left $ show err
        CryptoPassed publicKey -> Right PublicKey{..}
