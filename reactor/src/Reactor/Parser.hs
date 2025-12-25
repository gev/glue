module Reactor.Parser (
    Parser,
    parseReactor,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Reactor.AST (AST (..), Body (..))
import Reactor.Parser.Error (ParserError (..), parserError)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec ParserError Text

data SomeBody where
    SomeBody :: Body k -> SomeBody

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
    pure $ List (Atoms (Symbol "quote" : [inner]))

pNumber :: Parser AST
pNumber = Number <$> lexeme L.scientific

pString :: Parser AST
pString = String . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pSymbol :: Parser AST
pSymbol = do
    s <- lexeme $ T.pack <$> some (alphaNumChar <|> oneOf ("-._:!?" :: String))
    pure $ Symbol s

pExprOrList :: Parser AST
pExprOrList = between (symbol "(") (symbol ")") $ do
    optional pReactor >>= \case
        Nothing -> pure $ List (Atoms [])
        Just first -> case first of
            Symbol name | not (T.isPrefixOf ":" name) -> do
                SomeBody body <- pBodyRest []
                pure . List . Atoms $ case body of
                    Atoms atoms -> Symbol name : atoms
                    props -> [Symbol name, List props]
            _ -> do
                SomeBody body <- pBodyRest [first]
                pure $ List body

pBodyRest :: [AST] -> Parser SomeBody
pBodyRest initial = do
    elems <- (initial ++) <$> many pReactor
    case elems of
        [] -> pure $ SomeBody (Atoms [])
        (x : _) | isProp x -> do
            props <- validateProps elems
            pure $ SomeBody (Props props)
        _ -> do
            validateNoProps elems
            pure $ SomeBody (Atoms elems)
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
