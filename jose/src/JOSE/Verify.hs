module JOSE.Verify where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString
import Data.ByteString.Base64.URL
import JOSE.Header
import JOSE.JWT
import JOSE.PublicKey

verifySignature ::
    (Monad m) =>
    PublicKeys m ->
    ByteString ->
    ExceptT String m Token
verifySignature pks bs = do
    jwt <- splitToken bs
    token <- parseToken jwt
    signature <- except $ decodeUnpadded jwt.signature
    case Ed.signature signature of
        CryptoFailed err -> throwE $ show err
        CryptoPassed sig -> do
            let err = "Public key not found: " <> show token.header.kid
            pk <- maybeToExceptT err $ pks.findBy token.header.kid
            let msg = jwt.header <> "." <> jwt.payload
            if Ed.verify pk.publicKey msg sig
                then pure token
                else throwE "Invalid signature"
