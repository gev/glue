module Service.Register.Finish where

import Data.Aeson
import Data.Text
import Environment
import GHC.Generics

data FinishRegisterOptions = FinishRegisterOptions
    { id :: Text
    }
    deriving (Generic, Show)
instance FromJSON FinishRegisterOptions

data FinishRegisterResponse = FinishRegisterResponse
    { res :: Text
    }
    deriving (Generic, Show)
instance ToJSON FinishRegisterResponse

finishRegister :: (?environment :: Environment) => FinishRegisterOptions -> IO (Maybe FinishRegisterResponse)
finishRegister _ = pure . Just $ FinishRegisterResponse "Finish Register!"
