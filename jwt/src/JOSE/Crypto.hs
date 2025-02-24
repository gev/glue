module JOSE.Crypto where

import Control.Monad.Trans.Except
import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString (ByteString)
import Data.UUID
import Data.UUID.V4

data KeyPair = KeyPair
    { kid :: UUID
    , secretKey :: Ed.SecretKey
    , publicKey :: Ed.PublicKey
    }

makeKeyPair :: UUID -> ByteString -> ExceptT String IO KeyPair
makeKeyPair kid secretKey' = do
    secretKey <- withExceptT show . except . eitherCryptoError $ Ed.secretKey secretKey'
    let publicKey = Ed.toPublic secretKey
    pure
        KeyPair
            { kid
            , secretKey
            , publicKey
            }

generateKeyPair :: IO KeyPair
generateKeyPair = do
    kid <- nextRandom
    secretKey <- Ed.generateSecretKey
    let publicKey = Ed.toPublic secretKey
    pure
        KeyPair
            { kid
            , secretKey
            , publicKey
            }

sign :: KeyPair -> ByteString -> Ed.Signature
sign kp = Ed.sign kp.secretKey kp.publicKey
