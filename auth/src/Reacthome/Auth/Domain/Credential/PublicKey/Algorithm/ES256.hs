module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ES256 where

import Crypto.Error
import Crypto.Hash
import Crypto.PubKey.ECC.ECDSA qualified as ECDSA
import Crypto.PubKey.ECC.P256
import Crypto.PubKey.ECC.Types
import Data.ASN1.BitArray
import Data.ASN1.Prim
import Data.ByteString
import Util.ASN1
import Prelude hiding (splitAt)

decodePublicKey :: [ASN1] -> Either String ByteString
decodePublicKey = \case
    [ Start Sequence
        , Start Sequence
        , OID [1, 2, 840, 10045, 2, 1] -- EC public key
        , OID [1, 2, 840, 10045, 3, 1, 7] -- p256 curve
        , End Sequence
        , BitString (BitArray 520 bytes)
        , End Sequence
        ] -> do
            case uncons bytes of
                Just (pointFormat, pointBytes) ->
                    if pointFormat == 0x04
                        then Right pointBytes
                        else Left "Invalid p256 public key format: expected uncompressed point format (0x04)"
                Nothing -> Left "Invalid p256 public key format"
    _ -> Left "Invalid p256 public key format"

makePublicKey :: ByteString -> Either String ECDSA.PublicKey
makePublicKey bytes =
    case pointFromBinary bytes of
        CryptoPassed point -> do
            let (x, y) = pointToIntegers point
            Right $ ECDSA.PublicKey p256 $ Point x y
        _ -> Left "Invalid p256 public key format"
  where
    p256 = getCurveByName SEC_p256r1

decodeSignature :: [ASN1] -> Either String ECDSA.Signature
decodeSignature = \case
    [ Start Sequence
        , IntVal r
        , IntVal s
        , End Sequence
        ] -> Right $ ECDSA.Signature r s
    _ -> Left "Invalid ECDSA signature format"

verifySignature ::
    ByteString ->
    ByteString ->
    ByteString ->
    Either String Bool
verifySignature keyBytes message sigBytes = do
    key <- makePublicKey keyBytes
    signature <- decodeSignature =<< derDecode sigBytes
    pure $ ECDSA.verify SHA256 key signature message
