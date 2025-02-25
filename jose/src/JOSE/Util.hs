module JOSE.Util where

import Data.Aeson

aesonOptions :: Options
aesonOptions =
    defaultOptions
        { tagSingleConstructors = True
        }
