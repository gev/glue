module Util.Base64.Lazy where

import Control.Monad.Trans.Except
import Data.ByteString.Base64.Lazy
import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

toBase64 :: ByteString -> Text
toBase64 = decodeUtf8 . encode

fromBase64 :: (Monad m) => Text -> ExceptT String m ByteString
fromBase64 = except . decode . encodeUtf8
