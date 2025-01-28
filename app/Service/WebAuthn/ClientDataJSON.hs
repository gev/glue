module Service.WebAuthn.ClientDataJSON where

import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Util.Aeson
import Util.Base64.URL

data ClientDataJSON c = ClientDataJSON
    { type' :: Text
    , challenge :: c
    , origin :: Text
    , crossOrigin :: Bool
    }
    deriving (Show)

type EncodedClientDataJSON = ClientDataJSON Text

deriving instance Generic EncodedClientDataJSON

instance FromJSON EncodedClientDataJSON where
    parseJSON = genericParseJSON typeFieldLabelModifier

type DecodedClientDataJSON = ClientDataJSON ByteString

decodeClientDataJSON :: EncodedClientDataJSON -> Either String DecodedClientDataJSON
decodeClientDataJSON clientData = do
    challenge <- fromBase64 $ clientData.challenge
    pure
        ClientDataJSON
            { type' = clientData.type'
            , challenge
            , origin = clientData.origin
            , crossOrigin = clientData.crossOrigin
            }
