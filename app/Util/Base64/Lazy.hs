module Util.Base64.Lazy where

import Data.ByteString.Base64.Lazy (decode, encode)
import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Util.Error

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encode

fromBase64 :: Text -> Maybe ByteString
fromBase64 = hush . decode . encodeUtf8
