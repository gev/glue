module Reactor.Parser (
    parseReactor,
    parseReactorPretty,
    Parser,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Reactor.AST
import Reactor.Error (ReactorError (..), renderParserError)

-- Используем наш кастомный тип ошибки в Parsec
type Parser = Parsec ReactorError Text

-- Вспомогательная структура для гетерогенных тел
data SomeBody where
    SomeBody :: Body k -> SomeBody

-- | ГЛАВНАЯ ФУНКЦИЯ: Теперь возвращает чистый Either ReactorError Reactor
parseReactor :: Text -> Either ReactorError AST
parseReactor input =
    case parse (pReactor <* eof) "reactor-input" input of
        Left err -> Left (renderParserError err)
        Right ast -> Right ast

-- | Вспомогательная функция для вывода текста (например, в консоль)
parseReactorPretty :: Text -> Either Text AST
parseReactorPretty input =
    case parseReactor input of
        Left err -> Left (T.pack $ show err) -- Или можно вызвать showErrorComponent
        Right ast -> Right ast

-- --- Лексер ---

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- --- Парсеры атомов ---

pReactor :: Parser AST
pReactor =
    choice
        [ pQuoted -- 1. Добавляем сахар цитирования ПЕРВЫМ в список выбора
        , pExprOrList
        , pString
        , pNumber
        , pSymbol
        ]

pQuoted :: Parser AST
pQuoted = do
    _ <- char '\''
    -- Рекурсивно вызываем pReactor: теперь можно цитировать и символ, и список, и число
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
