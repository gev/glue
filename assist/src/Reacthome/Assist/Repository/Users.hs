module Reacthome.Assist.Repository.Users where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.HashMap.Strict
import Data.Text.Lazy
import Data.UUID
import GHC.Generics
import Reacthome.Assist.Domain.Server.Id
import Reacthome.Assist.Domain.User
import Reacthome.Assist.Domain.User.Id
import Reacthome.Assist.Domain.Users
import Prelude hiding (lookup)

makeUsers :: String -> IO Users
makeUsers file = do
    rows <-
        maybe
            (error $ "Can't read users from `" <> file <> "`")
            pure
            =<< decodeFileStrict file
    users' <- traverse fromUserRow rows
    let
        users =
            fromList $
                fmap (\user -> (user.id, user)) users'

        findById cid =
            hoistMaybe $
                lookup cid users
    pure
        Users
            { findById
            }

data UserRow = UserRow
    { user :: Text
    , servers :: [Text]
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

fromUserRow :: UserRow -> IO User
fromUserRow row = do
    uid <- toUUID "Invalid UUID format of user's id" row.user
    servers <- traverse (toUUID "Invalid UUID format of server's id") row.servers
    pure
        User
            { id = UserId uid
            , servers = ServerId <$> servers
            }

toUUID :: (Applicative a) => String -> LazyText -> a UUID
toUUID err text =
    maybe (error err) pure $
        fromText (toStrict text)
