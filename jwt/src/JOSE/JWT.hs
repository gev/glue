module JOSE.JWT where

import Control.Monad
import Control.Monad.Trans.Except
import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson
import Data.ByteArray.Encoding
import Data.ByteString.Base64.URL qualified as BS64
import Data.ByteString.Char8
import Data.Time (UTCTime)
import JOSE.Crypto
import JOSE.Header
import JOSE.Payload

data Token = Token
    { header :: Header
    , payload :: Payload
    }
    deriving stock (Show)

data JWT = JWT
    { header :: ByteString
    , payload :: ByteString
    , signature :: ByteString
    }

makeToken :: Header -> Payload -> Token
makeToken = Token

signToken :: KeyPair -> Token -> ByteString
signToken kp token =
    message <> "." <> signature
  where
    header = code token.header
    payload = code token.payload
    message = header <> "." <> payload
    signature = convertToBase Base64URLUnpadded $ sign kp message

    code :: (ToJSON a) => a -> ByteString
    code = BS64.encodeUnpadded . toStrict . encode

splitToken :: (Monad m) => ByteString -> ExceptT String m JWT
splitToken token = case split '.' token of
    [header, payload, signature] ->
        pure
            JWT
                { header
                , payload
                , signature
                }
    _ -> throwE "Invalid token format"

parseToken :: JWT -> ExceptT String IO Token
parseToken token = do
    header <- code token.header
    payload <- code token.payload
    pure
        Token
            { header
            , payload
            }
  where
    code :: (Monad m, FromJSON a) => ByteString -> ExceptT String m a
    code = except . (eitherDecode . fromStrict <=< BS64.decodeUnpadded)

verifyToken :: (Monad m) => Ed.PublicKey -> JWT -> ExceptT String m Bool
verifyToken public token = do
    signature' <- except $ BS64.decodeUnpadded token.signature
    let res = do
            signature'' <- Ed.signature signature'
            pure $ Ed.verify public (token.header <> "." <> token.payload) signature''
    withExceptT show . except . eitherCryptoError $ res

isTokenValid :: Token -> UTCTime -> Bool
isTokenValid token now = now < token.payload.exp
