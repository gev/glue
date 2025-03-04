module Util.Base64.URL.Lazy where

import Control.Monad.Trans.Except
import Data.ByteString.Base64.URL.Lazy
import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encodeUnpadded

fromBase64 :: (Monad m) => Text -> ExceptT String m ByteString
fromBase64 = except . decodeUnpadded . encodeUtf8
