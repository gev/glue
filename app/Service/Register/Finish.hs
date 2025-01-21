module Service.Register.Finish where

import Data.Aeson
import Data.Text
import Environment
import GHC.Generics

data FinishRegisterOptions = FinishRegisterOptions
    { id :: Text
    , authenticatorAttachment :: Text
    , response :: ResponseOptions
    }
    deriving (Generic, Show)
instance FromJSON FinishRegisterOptions

data ResponseOptions = ResponseOptions
    { attestationObject :: Text
    , clientDataJSON :: Text
    }
    deriving (Generic, Show)
instance FromJSON ResponseOptions

data FinishRegisterResponse = FinishRegisterResponse
    { res :: Text
    }
    deriving (Generic, Show)
instance ToJSON FinishRegisterResponse

finishRegister ::
    (?environment :: Environment) =>
    FinishRegisterOptions ->
    IO (Maybe FinishRegisterResponse)
finishRegister req = do
    print req
    pure . Just $ FinishRegisterResponse "Finish Register!"
