module Util.ByteString (
    ByteString,
) where

import Data.Aeson
import Data.ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.Text.Encoding

instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8 . Base64.encode
