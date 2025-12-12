module Reacthome.Auth.Domain.Credential.PublicKey.Algorithm.RS256 where

import Crypto.Number.Serialize
import Data.ASN1.BitArray
import Data.ASN1.Prim
import Data.ByteString
import Data.ByteString.Builder
import Util.ASN1 qualified as ASN1
import Prelude hiding (length)

decodePublicKey :: [ASN1] -> Either String ByteString
decodePublicKey = \case
    [ Start Sequence
        , Start Sequence
        , OID [1, 2, 840, 113549, 1, 1, 1] -- rsaEncryption OID
        , Null
        , End Sequence
        , BitString (BitArray _ keyBytes)
        , End Sequence
        ] -> decodeRSAKey keyBytes
    _ -> Left "Invalid RSA public key format"

decodeRSAKey :: ByteString -> Either String ByteString
decodeRSAKey keyBytes = do
    bytes <- ASN1.berDecode keyBytes
    case bytes of
        [Start Sequence, IntVal n, IntVal e, End Sequence] ->
            Right $
                toStrict $
                    toLazyByteString $
                        mconcat
                            [ encodeMPI n -- modulus
                            , encodeMPI e -- public exponent
                            ]
        _ -> Left "Invalid RSA public key format"

encodeMPI :: Integer -> Builder
encodeMPI i =
    let bytes = i2osp i
        len = fromIntegral $ length bytes
     in mconcat
            [ byteString $ i2osp len
            , byteString bytes
            ]

verifySignature ::
    ByteString ->
    ByteString ->
    ByteString ->
    Either String Bool
verifySignature _ _ _ = Left "RS256 verify signature not implemented"

{-
    TODO: WebAuthn. Implement RS256
-}
