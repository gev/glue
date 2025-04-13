module Reacthome.Assist.Domain.Answer where

import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics
import Util.Aeson

type AnswerID = UUID

data Answer = Answer
    { message :: Text
    , session :: Text
    , end :: Maybe Bool
    }
    deriving stock (Generic, Show)

instance FromJSON Answer where
    parseJSON = genericParseJSON omitNothing
