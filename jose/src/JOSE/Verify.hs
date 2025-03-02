module JOSE.Verify where

import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString
import Data.ByteString.Base64.URL
import JOSE.Header
import JOSE.JWT
import JOSE.PublicKey

verify :: PublicKeys -> ByteString -> Either String Bool
verify pks bs = do
    jwt <- splitToken bs
    token <- parseToken jwt
    signature <- decodeUnpadded jwt.signature
    case Ed.signature signature of
        CryptoFailed err -> Left $ show err
        CryptoPassed sig -> do
            let err = "Public key not found: " <> show token.header.kid
            pk <-
                maybe (Left err) Right $ pks.findBy token.header.kid
            let msg = jwt.header <> "." <> jwt.payload
            Right $ Ed.verify pk.publicKey msg sig
