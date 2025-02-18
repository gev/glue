module Util.ASN1 where

import Control.Monad.Trans.Except
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Prim
import Data.ByteString

decode :: (Monad m) => ByteString -> ExceptT String m [ASN1]
decode bs = except $
    case decodeASN1' DER bs of
        Left err -> Left $ show err
        Right asn1 -> Right asn1
