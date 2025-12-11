module Util.Base64.Lazy where

import Data.ByteString.Base64.Lazy
import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encode

fromBase64 :: Text -> Either String ByteString
fromBase64 = decode . encodeUtf8
