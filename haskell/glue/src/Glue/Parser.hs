module Glue.Parser (
    Parser,
    parseGlue,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Glue.AST (AST (..))
import Glue.Parser.Error (ParserError (..), parserError)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec ParserError Text

parseGlue :: Text -> Either ParserError AST
parseGlue input =
    case parse (pGlue <* eof) "glue-input" input of
        Left err -> Left (parserError err)
        Right ast -> Right ast

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pGlue :: Parser AST
pGlue =
    choice
        [ pExprOrList
        , pString
        , pNumber
        , pSymbol
        ]

pNumber :: Parser AST
pNumber = try $ Number <$> lexeme (L.signed (pure ()) L.scientific)

pString :: Parser AST
pString = String . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pSymbol :: Parser AST
pSymbol = Symbol . T.pack <$> lexeme (some (alphaNumChar <|> oneOf ("-._:!?\\=<>/*+%$@#&|'" :: String)))

pExprOrList :: Parser AST
pExprOrList = between (symbol "(") (symbol ")") $ do
    optional pGlue >>= \case
        Nothing -> pure $ List []
        Just first -> case first of
            Symbol name | not (T.isPrefixOf ":" name) -> do
                body <- pBodyRest []
                case body of
                    List atoms -> pure $ List (Symbol name : atoms)
                    propList -> pure $ List [Symbol name, propList]
            _ -> pBodyRest [first]

pBodyRest :: [AST] -> Parser AST
pBodyRest initial = do
    elems <- (initial <>) <$> many pGlue
    case elems of
        [] -> pure $ List []
        (x : _) | isProp x -> do
            props <- validateProps elems
            pure $ Object props
        _ -> do
            validateNoProps elems
            pure $ List elems
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
