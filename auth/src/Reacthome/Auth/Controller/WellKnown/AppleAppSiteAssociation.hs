module Reacthome.Auth.Controller.WellKnown.AppleAppSiteAssociation where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics
import Reacthome.Auth.Environment
import Web.Rest
import Web.Rest.Media (toJSON)

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

appleAppSiteAssociation :: (?environment :: Environment) => ExceptT String IO Response
appleAppSiteAssociation = do
    lift $ print @String "Apple"
    toJSON $
        AppleAppSiteAssociation
            { webcredentials =
                WebCredentials
                    { apps = ?environment.appleApps
                    }
            }
