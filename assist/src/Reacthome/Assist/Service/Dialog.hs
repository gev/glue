module Reacthome.Assist.Service.Dialog where

import Data.Aeson
import Data.Text
import GHC.Generics
import Reacthome.Assist.Domain.Answer
import Reacthome.Assist.Domain.Query
import Util.Aeson

data Answers = Answers
    { takeAnswer :: AnswerID -> IO Answer
    , putAnswer :: AnswerID -> Answer -> IO (Maybe ())
    }

type GetAnswer = Query -> IO Answer

type SetAnswer = AnswerID -> Answer -> IO ()

data Container p = Container
    { uid :: AnswerID
    , type' :: Text
    , payload :: p
    }
    deriving stock (Generic, Show)

instance (FromJSON p) => FromJSON (Container p) where
    parseJSON = genericParseJSON typeFieldLabelModifier

instance (ToJSON p) => ToJSON (Container p) where
    toJSON = genericToJSON typeFieldLabelModifier

pack :: AnswerID -> p -> Container p
pack uid payload =
    Container
        { uid
        , type' = "ACTION_ASSIST"
        , payload
        }

unpack :: Container p -> Either String (AnswerID, p)
unpack c = case c.type' of
    "ACTION_ASSIST" -> Right (c.uid, c.payload)
    _ -> Left "Unknown action type"
