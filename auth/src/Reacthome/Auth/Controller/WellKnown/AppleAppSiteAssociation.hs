module Reacthome.Auth.Controller.WellKnown.AppleAppSiteAssociation where

import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics
import Reacthome.Auth.Environment
import Rest
import Rest.Media (toJSON)

newtype AppleAppSiteAssociation = AppleAppSiteAssociation
    { webcredentials :: WebCredentials
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

newtype WebCredentials = WebCredentials
    { apps :: [Text]
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

appleAppSiteAssociation ::
    (?environment :: Environment) => IO Response
appleAppSiteAssociation =
    toJSON $
        AppleAppSiteAssociation
            { webcredentials =
                WebCredentials
                    { apps = ?environment.appleApps
                    }
            }
