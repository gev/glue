module Reacthome.Auth.Repository.Clients where

import Control.Monad.Trans.Maybe (hoistMaybe)
import Crypto.Random
import Data.Aeson (FromJSON, decodeFileStrict)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Base64.Lazy (decode)
import Data.ByteString.Lazy as B hiding (filter)
import Data.HashMap.Strict
import Data.Text.Lazy as T hiding (filter)
import Data.Text.Lazy.Encoding
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import Reacthome.Auth.Domain.Client
import Reacthome.Auth.Domain.Client.Id
import Reacthome.Auth.Domain.Client.Name
import Reacthome.Auth.Domain.Clients
import Reacthome.Auth.Domain.Hash
import Prelude hiding (lookup)

makeClients :: String -> IO Clients
makeClients file = do
    rows <-
        maybe
            (error $ "Can't read clients from `" <> file <> "`")
            pure
            =<< decodeFileStrict file
    clients' <- traverse fromClientRow rows
    let
        clients =
            fromList $
                fmap (\client -> (client.id, client)) clients'

        findById cid =
            hoistMaybe $
                lookup cid clients
    pure
        Clients
            { findById
            }

data ClientRow = ClientRow
    { id :: Text
    , name :: Text
    , secret :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

fromClientRow :: ClientRow -> IO Client
fromClientRow row = do
    cid <-
        maybe (error "Invalid UUID format of user's id") pure $
            fromText (T.toStrict row.id)
    secret <-
        either error pure $
            decode (encodeUtf8 row.secret)
    pure
        Client
            { id = ClientId cid
            , name = ClientName $ T.toStrict row.name
            , secret = Hash $ B.toStrict secret
            }

generateClient :: IO ()
generateClient = do
    cid <- nextRandom
    secret <- getRandomBytes 20
    let hash = makeHash secret
    print $ "id: " <> toString cid
    print $ "secret: " <> encode secret
    print $ "hash: " <> encode hash.value
