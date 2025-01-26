module Util.Base64.URL where

import Data.ByteString
import Data.ByteString.Base64.URL (decode, encode)
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Util.Error

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encode

fromBase64 :: Text -> Maybe ByteString
fromBase64 = hush . decode . encodeUtf8
