module JOSE.JWT where

import Data.Aeson
import Data.ByteString.Base64.URL (decodeUnpadded)
import Data.ByteString.Char8
import Data.Time.Clock.POSIX
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

splitToken :: ByteString -> Either String JWT
splitToken token = case split '.' token of
    [header, payload, signature] ->
        Right JWT{..}
    _ -> Left "Invalid token format"

parseToken :: JWT -> Either String Token
parseToken token = do
    header <- code token.header
    payload <- code token.payload
    pure Token{..}
  where
    code :: (FromJSON a) => ByteString -> Either String a
    code bs = eitherDecode . fromStrict =<< decodeUnpadded bs

isTokenValid :: Token -> POSIXTime -> Bool
isTokenValid token now = round now < token.payload.exp

isTokenValidNow :: Token -> IO Bool
isTokenValidNow token = isTokenValid token <$> getPOSIXTime
