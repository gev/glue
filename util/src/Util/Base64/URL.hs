module Util.Base64.URL where

import Data.ByteString
import Data.ByteString.Base64.URL
import Data.Text
import Data.Text.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encodeUnpadded

fromBase64 :: Text -> Either String ByteString
fromBase64 = decodeUnpadded . encodeUtf8
