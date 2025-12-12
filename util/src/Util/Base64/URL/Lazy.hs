module Util.Base64.URL.Lazy where

import Data.ByteString.Base64.URL.Lazy
import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encodeUnpadded

fromBase64 :: Text -> Either String ByteString
fromBase64 = decodeUnpadded . encodeUtf8
