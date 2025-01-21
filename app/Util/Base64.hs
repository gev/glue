module Util.Base64 where

import Data.ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.Text
import Data.Text.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . Base64.encode

fromBase64 :: Text -> Either String ByteString
fromBase64 = Base64.decode . encodeUtf8
