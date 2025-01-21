module Util.Base64 where

import Data.ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.Text
import Data.Text.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . Base64.encode

fromBase64 :: Text -> Maybe ByteString
fromBase64 = hush . Base64.decode . encodeUtf8
  where
    hush = \case
        Right b -> Just b
        _ -> Nothing
