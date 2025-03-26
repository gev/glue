module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.ES256 where

import Control.Monad.Trans.Except
import Crypto.Number.Serialize
import Crypto.PubKey.ECC.ECDSA qualified as ECDSA
import Crypto.PubKey.ECC.Types
import Data.ASN1.BitArray
import Data.ASN1.Prim
import Data.ByteString
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

makePublicKey :: ByteString -> ECDSA.PublicKey
makePublicKey bytes =
    ECDSA.PublicKey p256 point
  where
    p256 = getCurveByName SEC_p256k1
    point = Point x y
    x = os2ip xBytes
    y = os2ip yBytes
    (xBytes, yBytes) = splitAt 32 bytes
