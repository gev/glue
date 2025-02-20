module Reacthome.Assist.Service.Dialog where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Maybe
import Data.Text
import Data.Text.Lazy.Encoding
import Data.UUID
import GHC.Generics
import Reacthome.Assist.Domain.Answer
import Reacthome.Assist.Domain.Query
import Reacthome.Gate.Connection
import Reacthome.Gate.Connection.Pool
import Util.Aeson

data ActionQuery = ActionQuery
    { type' :: Text
    , message :: Text
    , sessionId :: Text
    }
    deriving stock (Generic, Show)

instance ToJSON ActionQuery where
    toJSON = genericToJSON typeFieldLabelModifier

makeActionQuery :: Text -> Text -> ActionQuery
makeActionQuery = ActionQuery "ACTION_ASSIST"

getAnswer ::
    (?gateConnectionPool :: GateConnectionPool) =>
    Query ->
    ExceptT String IO Answer
getAnswer query = do
    connection <- lift $ ?gateConnectionPool.getConnection myDaemon
    lift $
        connection.send
            ( decodeUtf8 . encode $
                makeActionQuery
                    query.message
                    query.sessionId
            )
    pure
        Answer
            { message = query.message
            , sessionId = query.sessionId
            , endSession = False
            }

myDaemon :: UUID
myDaemon = fromJust $ fromString "02aaee3f-a050-43d5-bbf2-e0f2abd73a6e"
