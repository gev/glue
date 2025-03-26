module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ES256 where

import Control.Monad.Trans.Except
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

decodePublicKey :: (Monad m) => [ASN1] -> ExceptT String m ByteString
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
                        then pure pointBytes
                        else throwE "Invalid p256 public key format: expected uncompressed point format (0x04)"
                _ -> throwE "Invalid p256 public key format"
    _ -> throwE "Invalid p256 public key format"

makePublicKey :: (Monad m) => ByteString -> ExceptT String m ECDSA.PublicKey
makePublicKey bytes =
    case pointFromBinary bytes of
        CryptoPassed point -> do
            let (x, y) = pointToIntegers point
            pure $ ECDSA.PublicKey p256 $ Point x y
        _ -> throwE "Invalid p256 public key format"
  where
    p256 = getCurveByName SEC_p256r1

decodeSignature ::
    (Monad m) =>
    [ASN1] ->
    ExceptT String m ECDSA.Signature
decodeSignature = \case
    [ Start Sequence
        , IntVal r
        , IntVal s
        , End Sequence
        ] -> pure $ ECDSA.Signature r s
    _ -> throwE "Invalid ECDSA signature format"

verifySignature ::
    (Monad m) =>
    ByteString ->
    ByteString ->
    ByteString ->
    ExceptT String m Bool
verifySignature keyBytes message sigBytes = do
    key <- makePublicKey keyBytes
    signature <- decodeSignature =<< derDecode sigBytes
    pure $ ECDSA.verify SHA256 key signature message
