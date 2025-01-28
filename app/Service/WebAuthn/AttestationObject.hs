module Service.WebAuthn.AttestationObject where

import Codec.CBOR.Read
import Codec.CBOR.Term
import Data.ByteString
import Data.ByteString.Lazy qualified as Lazy
import Data.Map
import Data.Map qualified as Map
import Data.Text

type AttestationFormat = Text

type AttestationStatement = Map Text Term

type AuthenticatorData = ByteString

data AttestationObject = AttestationObject
    { fmt :: AttestationFormat
    , attStmt :: AttestationStatement
    , authData :: AuthenticatorData
    }
    deriving (Show)

decodeAttestationObject :: Lazy.ByteString -> Either String AttestationObject
decodeAttestationObject cborBytes =
    case deserialiseFromBytes decodeTerm cborBytes of
        Left err -> Left $ show err
        Right (_, TMap kvs) -> do
            fmt <- decodeAttestationFormat kvs
            attStmt <- decodeAttestationStatement kvs
            authDataBytes <- decodeAuthenticatorData kvs
            Right $
                AttestationObject
                    { attStmt
                    , authData = authDataBytes
                    , fmt
                    }
        _ -> Left "Top-level CBOR is not a map"

decodeAttestationFormat :: [(Term, Term)] -> Either String Text
decodeAttestationFormat kvs = do
    term <- lookupTerm "fmt" kvs
    case term of
        TString txt -> Right txt
        _ -> Left "fmt is not text"

decodeAttestationStatement :: [(Term, Term)] -> Either String (Map Text Term)
decodeAttestationStatement kvs = do
    term <- lookupTerm "attStmt" kvs
    case term of
        TMap m -> Right $ Map.fromList [(t, v) | (TString t, v) <- m]
        _ -> Left "attStmt is not a map"

decodeAuthenticatorData :: [(Term, Term)] -> Either String AuthenticatorData
decodeAuthenticatorData kvs = do
    term <- lookupTerm "authData" kvs
    case term of
        TBytes bs -> Right bs
        _ -> Left "authData is not bytes"

lookupTerm :: Text -> [(Term, Term)] -> Either String Term
lookupTerm key kvs =
    case [v | (TString k, v) <- kvs, k == key] of
        (v : _) -> Right v
        [] -> Left $ "Key not found: " ++ show key
