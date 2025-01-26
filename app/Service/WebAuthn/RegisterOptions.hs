module Service.WebAuthn.RegisterOptions where

import Control.Monad
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
    RegisterOptions -> Maybe ValidRegisterOptions
validateRegisterOptions options = do
    let name = strip options.name
    guard $ validName name
    let displayName = strip options.displayName
    guard $ validName displayName
    pure
        ValidRegisterOptions
            { options =
                RegisterOptions
                    { name
                    , displayName
                    }
            }

validName :: Text -> Bool
validName = not . null
