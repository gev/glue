module JOSE.JWT where

import Control.Monad.Trans.Except
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

parseToken :: (Monad m) => JWT -> ExceptT String m Token
parseToken token = do
    header <- code token.header
    payload <- code token.payload
    pure
        Token
            { header
            , payload
            }
  where
    code :: (FromJSON a, Monad m) => ByteString -> ExceptT String m a
    code bs = except $ eitherDecode . fromStrict =<< decodeUnpadded bs

isTokenValid :: Token -> POSIXTime -> Bool
isTokenValid token now = round now < token.payload.exp

isTokenValidNow :: Token -> IO Bool
isTokenValidNow token = isTokenValid token <$> getPOSIXTime
