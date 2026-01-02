module Reactor.Parser (
    Parser,
    parseReactor,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Reactor.AST (AST (..))
import Reactor.Parser.Error (ParserError (..), parserError)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec ParserError Text

parseReactor :: Text -> Either ParserError AST
parseReactor input =
    case parse (pReactor <* eof) "reactor-input" input of
        Left err -> Left (parserError err)
        Right ast -> Right ast

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pReactor :: Parser AST
pReactor =
    choice
        [ pQuoted
        , pExprOrList
        , pString
        , pNumber
        , pSymbol
        ]

pQuoted :: Parser AST
pQuoted = do
    _ <- char '\''
    inner <- pReactor
    pure $ AtomList [Symbol "quote", inner]

pNumber :: Parser AST
pNumber = try $ do
    num <- Number <$> lexeme (L.signed (pure ()) L.scientific)
    notFollowedBy (alphaNumChar <|> oneOf ("-._:!?\\=<>/*+%$@#&|'" :: String))
    pure num

pString :: Parser AST
pString = String . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pSymbol :: Parser AST
pSymbol = Symbol . T.pack <$> lexeme (some (alphaNumChar <|> oneOf ("-._:!?\\=<>/*+%$@#&|'" :: String)))

pExprOrList :: Parser AST
pExprOrList = between (symbol "(") (symbol ")") $ do
    optional pReactor >>= \case
        Nothing -> pure $ AtomList []
        Just first -> case first of
            Symbol name | not (T.isPrefixOf ":" name) -> do
                body <- pBodyRest []
                case body of
                    AtomList atoms -> pure $ AtomList (Symbol name : atoms)
                    propList -> pure $ AtomList [Symbol name, propList]
            _ -> pBodyRest [first]

pBodyRest :: [AST] -> Parser AST
pBodyRest initial = do
    elems <- (initial <>) <$> many pReactor
    case elems of
        [] -> pure $ AtomList []
        (x : _) | isProp x -> do
            props <- validateProps elems
            pure $ PropList props
        _ -> do
            validateNoProps elems
            pure $ AtomList elems
  where
    isProp (Symbol s) = T.isPrefixOf ":" s
    isProp _ = False

validateProps :: [AST] -> Parser [(Text, AST)]
validateProps = \case
    [] -> pure []
    [Symbol k] | T.isPrefixOf ":" k -> customFailure (UnpairedProperty k)
    (Symbol k : v : rest) | T.isPrefixOf ":" k -> do
        others <- validateProps rest
        pure ((T.drop 1 k, v) : others)
    (x : _) -> customFailure (MixedContent (T.pack $ show x))

validateNoProps :: [AST] -> Parser ()
validateNoProps = mapM_ \case
    Symbol s | T.isPrefixOf ":" s -> customFailure (MixedContent s)
    _ -> pure ()
