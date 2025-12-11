module Util.ASN1 where

import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Prim
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)

decode ::
    (ASN1Decoding a) =>
    a -> ByteString -> Either String [ASN1]
decode a bs = first show $ decodeASN1' a bs

berDecode :: ByteString -> Either String [ASN1]
berDecode = decode BER

derDecode :: ByteString -> Either String [ASN1]
derDecode = decode DER
