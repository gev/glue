module JOSE.Sign where

import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson
import Data.ByteArray.Encoding
import Data.ByteString
import Data.ByteString.Base64.URL (encodeUnpadded)
import Data.Text
import Data.UUID
import JOSE.Header
import JOSE.JWT
import JOSE.KeyPair
import JOSE.Payload

generateToken :: KeyPair -> Text -> Int -> UUID -> IO ByteString
generateToken kp iss ttl sub = do
    let header = makeHeader kp.kid
    payload <- newPayload iss ttl sub
    let token = makeToken header payload
    pure $ signToken kp token

signToken :: KeyPair -> Token -> ByteString
signToken kp token =
    message <> "." <> signature
  where
    header = code token.header
    payload = code token.payload
    message = header <> "." <> payload
    signature = convertToBase Base64URLUnpadded $ Ed.sign kp.secretKey kp.publicKey message

    code :: (ToJSON a) => a -> ByteString
    code = encodeUnpadded . toStrict . encode
