module JOSE.Verify where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
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
    m (Either String Token)
verifySignature pks bs = runExceptT do
    jwt <- except (splitToken bs)
    token <- except (parseToken jwt)
    signature <- except (decodeUnpadded jwt.signature)
    case Ed.signature signature of
        CryptoFailed err -> throwE (show err)
        CryptoPassed sig -> do
            pk <- except =<< lift (pks.findBy token.header.kid)
            let msg = jwt.header <> "." <> jwt.payload
            if Ed.verify pk.publicKey msg sig
                then pure token
                else throwE "Invalid signature"
