module Service.Register.Finish where

import Data.Aeson
import Data.Text
import Data.Text.Lazy qualified as Lazy
import Environment
import GHC.Generics
import Util.Aeson
import Util.Base64
import Util.Base64.Lazy qualified as Lazy

data PublicKeyCredential = PublicKeyCredential
    { id :: Text
    , authenticatorAttachment :: Text
    , response :: ResponseOptions
    }
    deriving (Generic, Show)
instance FromJSON PublicKeyCredential

data ResponseOptions = ResponseOptions
    { attestationObject :: Text
    , clientDataJSON :: Lazy.Text
    }
    deriving (Generic, Show)
instance FromJSON ResponseOptions

data ClientDataJSON = ClientDataJSON
    { type' :: Text
    , challenge :: Text
    , origin :: Text
    , crossOrigin :: Bool
    }
    deriving (Generic, Show)
instance FromJSON ClientDataJSON where
    parseJSON = genericParseJSON typeFieldLabelModifier
data FinishRegisterResponse = FinishRegisterResponse
    { res :: Text
    }
    deriving (Generic, Show)
instance ToJSON FinishRegisterResponse

finishRegister ::
    (?environment :: Environment) =>
    PublicKeyCredential ->
    IO (Maybe FinishRegisterResponse)
finishRegister req = do
    print req
    print $ fromBase64 req.id
    print $ fromBase64 req.response.attestationObject
    let cdata = decode =<< Lazy.fromBase64 req.response.clientDataJSON
    print $ (cdata :: Maybe ClientDataJSON)
    pure . Just $ FinishRegisterResponse "Finish Register!"
