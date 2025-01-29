module Reacthome.Auth.Service.WebAuthn.RegisterOptions where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prelude hiding (null)

data RegisterOptions = RegisterOptions
    { name :: Text
    , displayName :: Text
    }
    deriving (Generic, Show, FromJSON)

newtype ValidRegisterOptions = ValidRegisterOptions
    { options :: RegisterOptions
    }
    deriving (Show)

validateRegisterOptions ::
    RegisterOptions -> Either String ValidRegisterOptions
validateRegisterOptions options = do
    name <- validateName options.name
    displayName <- validateDisplayName options.displayName
    pure
        ValidRegisterOptions
            { options =
                RegisterOptions
                    { name
                    , displayName
                    }
            }

validateName :: Text -> Either String Text
validateName = validate "name"

validateDisplayName :: Text -> Either String Text
validateDisplayName = validate "displayName"

validate :: String -> Text -> Either String Text
validate field name = do
    let name' = strip name
    if not $ null name'
        then Right name'
        else Left $ "The " <> field <> " cannot be empty"
