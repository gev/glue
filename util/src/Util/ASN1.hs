module Util.ASN1 where

import Control.Monad.Trans.Except
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Prim
import Data.ByteString (ByteString)

decode ::
    (Monad m, ASN1Decoding a) =>
    a ->
    ByteString ->
    ExceptT String m [ASN1]
decode a bs = except $
    case decodeASN1' a bs of
        Left err -> Left $ show err
        Right asn1 -> Right asn1

berDecode ::
    (Monad m) =>
    ByteString ->
    ExceptT String m [ASN1]
berDecode = decode BER

derDecode :: (Monad m) => ByteString -> ExceptT String m [ASN1]
derDecode = decode DER
