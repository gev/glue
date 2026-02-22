module Glue.Parse (
    Parse,
    parseGlue,
) where

import Control.Monad (guard)
import Data.Text (Text)
import Data.Text qualified as T
import Glue.AST (AST (..))
import Glue.Parse.Error (ParseError (..), parserError)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), Parsec, between, choice, customFailure, many, manyTill, oneOf, optional, parse, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parse = Parsec ParseError Text

parseGlue :: Text -> Either ParseError AST
parseGlue input =
    case parse (pGlue <* eof) "glue-input" input of
        Left err -> Left (parserError err)
        Right ast -> Right ast

sc :: Parse ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parse a -> Parse a
lexeme = L.lexeme sc

symbol :: Text -> Parse Text
symbol = L.symbol sc

pQuote :: Parse AST
pQuote = do
    _ <- char '\''
    expr <- pGlue
    pure $ List [Symbol "quote", expr]

pGlue :: Parse AST
pGlue =
    choice
        [ pQuote
        , pExprOrList
        , pString
        , pInteger
        , pFloat
        , pSymbol
        ]

pInteger :: Parse AST
pInteger = try $ do
    n <- lexeme (L.signed (pure ()) L.decimal)
    notFollowedBy (char '.')
    pure $ Integer n

pFloat :: Parse AST
pFloat = try $ do
    n <- lexeme (L.signed (pure ()) L.scientific)
    let str = show n
    guard ('.' `elem` str || 'e' `elem` str || 'E' `elem` str)
    pure $ Float (fromRational $ toRational n)

pString :: Parse AST
pString = String . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pSymbol :: Parse AST
pSymbol = Symbol . T.pack <$> lexeme (some (alphaNumChar <|> oneOf ("-._:!?\\=<>/*+%$@#&|'" :: String)))

pExprOrList :: Parse AST
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

pBodyRest :: [AST] -> Parse AST
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

validateProps :: [AST] -> Parse [(Text, AST)]
validateProps = \case
    [] -> pure []
    [Symbol k] | T.isPrefixOf ":" k -> customFailure (UnpairedProperty k)
    (Symbol k : v : rest) | T.isPrefixOf ":" k -> do
        others <- validateProps rest
        pure ((T.drop 1 k, v) : others)
    (x : _) -> customFailure (MixedContent (T.pack $ show x))

validateNoProps :: [AST] -> Parse ()
validateNoProps = mapM_ \case
    Symbol s | T.isPrefixOf ":" s -> customFailure (MixedContent s)
    _ -> pure ()
