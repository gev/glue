module Util.Aeson where

import Data.Aeson

omitNothing :: Options
omitNothing =
    defaultOptions
        { omitNothingFields = True
        }

typeFieldLabelModifier :: Options
typeFieldLabelModifier =
    defaultOptions
        { fieldLabelModifier = \s ->
            if s == "type'"
                then "type"
                else s
        }
