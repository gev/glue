module Util.Base64.URL where

import Control.Monad.Trans.Except
import Data.ByteString
import Data.ByteString.Base64.URL
import Data.Text
import Data.Text.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encode

fromBase64 :: (Monad m) => Text -> ExceptT String m ByteString
fromBase64 = except . decode . encodeUtf8
