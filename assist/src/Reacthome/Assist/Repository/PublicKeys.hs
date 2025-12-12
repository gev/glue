module Reacthome.Assist.Repository.PublicKeys where

import Control.Error (note)
import Data.HashMap.Strict
import Data.IORef
import JOSE.PublicKey
import Prelude hiding (lookup)

makePublicKeys :: IO (PublicKeys IO)
makePublicKeys = do
    jwks <- newIORef empty
    let
        findBy uid =
            note ("Public key " <> show uid <> " not found")
                . lookup uid
                <$> readIORef jwks
        store =
            writeIORef jwks
                . fromList
                . fmap \key -> (key.kid, key)
    pure
        PublicKeys{..}
