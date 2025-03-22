module Reacthome.Assist.Repository.PublicKeys where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict
import Data.IORef
import JOSE.PublicKey
import Prelude hiding (lookup)

makePublicKeys :: IO (PublicKeys IO)
makePublicKeys = do
    jwks <- newIORef empty
    let
        findBy uid = hoistMaybe . lookup uid =<< lift (readIORef jwks)
        store = writeIORef jwks . fromList . fmap \key -> (key.kid, key)
    pure
        PublicKeys
            { findBy
            , store
            }
