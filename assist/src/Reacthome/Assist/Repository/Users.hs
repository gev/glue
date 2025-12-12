module Reacthome.Assist.Repository.Users where

import Control.Error.Util (exceptT, (!?))
import Control.Monad.Trans.Class (lift)
import Data.Aeson
import Data.HashMap.Strict (fromList, lookup)
import Data.Text.Lazy (Text, toStrict)
import Data.UUID
import GHC.Generics
import Reacthome.Assist.Domain.Server.Id
import Reacthome.Assist.Domain.User
import Reacthome.Assist.Domain.User.Id
import Reacthome.Assist.Domain.Users
import Prelude hiding (lookup)

makeUsers :: String -> IO Users
makeUsers file = exceptT error pure do
    rows <- decodeFileStrict file !? ("Can't read users from `" <> file <> "`")
    users' <- lift $ traverse fromUserRow rows
    let
        users =
            fromList $
                fmap (\user -> (user.id, user)) users'

        findById cid = case lookup cid users of
            Nothing -> Left $ "User " <> show cid.value <> " not found"
            Just user -> Right user
    pure Users{..}

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

toUUID :: String -> Text -> IO UUID
toUUID err text =
    maybe (error err) pure $
        fromText (toStrict text)
