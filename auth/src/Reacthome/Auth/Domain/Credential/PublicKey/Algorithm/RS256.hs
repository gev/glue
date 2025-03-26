module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.RS256 where

import Control.Monad.Trans.Except
import Crypto.Number.Serialize
import Data.ASN1.BitArray
import Data.ASN1.Prim
import Data.ByteString
import Data.ByteString.Builder
import Util.ASN1 qualified as ASN1
import Prelude hiding (length)

decodePublicKey :: (Monad m) => [ASN1] -> ExceptT String m ByteString
decodePublicKey = \case
    [ Start Sequence
        , Start Sequence
        , OID [1, 2, 840, 113549, 1, 1, 1] -- rsaEncryption OID
        , Null
        , End Sequence
        , BitString (BitArray _ keyBytes)
        , End Sequence
        ] -> decodeRSAKey keyBytes
    _ -> throwE "Invalid RSA public key format"

decodeRSAKey :: (Monad m) => ByteString -> ExceptT String m ByteString
decodeRSAKey keyBytes = do
    bytes <- ASN1.berDecode keyBytes
    case bytes of
        [Start Sequence, IntVal n, IntVal e, End Sequence] ->
            pure $
                toStrict $
                    toLazyByteString $
                        mconcat
                            [ encodeMPI n -- modulus
                            , encodeMPI e -- public exponent
                            ]
        _ -> throwE "Invalid RSA public key format"

encodeMPI :: Integer -> Builder
encodeMPI i =
    let bytes = i2osp i
        len = fromIntegral $ length bytes
     in mconcat
            [ byteString $ i2osp len
            , byteString bytes
            ]

verifySignature ::
    (Monad m) =>
    ByteString ->
    ByteString ->
    ByteString ->
    ExceptT String m Bool
verifySignature _ _ _ = pure False

{-
    TODO: WebAuthn. Implement RS256
-}
