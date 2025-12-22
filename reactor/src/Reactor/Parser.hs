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
data SomeRBody where
    SomeRBody :: RBody k -> SomeRBody

-- | ГЛАВНАЯ ФУНКЦИЯ: Теперь возвращает чистый Either ReactorError Reactor
parseReactor :: Text -> Either ReactorError Reactor
parseReactor input =
    case parse (pReactor <* eof) "reactor-input" input of
        Left err -> Left (renderParserError err)
        Right ast -> Right ast

-- | Вспомогательная функция для вывода текста (например, в консоль)
parseReactorPretty :: Text -> Either Text Reactor
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

pReactor :: Parser Reactor
pReactor =
    choice
        [ pQuoted -- 1. Добавляем сахар цитирования ПЕРВЫМ в список выбора
        , pExprOrList
        , pString
        , pNumber
        , pSymbol
        ]

pQuoted :: Parser Reactor
pQuoted = do
    _ <- char '\''
    -- Рекурсивно вызываем pReactor: теперь можно цитировать и символ, и список, и число
    inner <- pReactor
    pure $ RExpr "quote" (RAtoms [inner])

pNumber :: Parser Reactor
pNumber = RNumber <$> lexeme L.scientific

pString :: Parser Reactor
pString = RString . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pSymbol :: Parser Reactor
pSymbol = do
    s <- lexeme $ T.pack <$> some (alphaNumChar <|> oneOf ("-._:!?" :: String))
    pure $ RSymbol s

-- --- Парсинг структур ( ... ) ---

pExprOrList :: Parser Reactor
pExprOrList = between (symbol "(") (symbol ")") $ do
    optional pReactor >>= \case
        -- Пустые скобки () превращаются в пустой список атомов
        Nothing -> pure $ RList (RAtoms [])
        Just first -> case first of
            -- Если первый элемент — не ключ, это вызов функции (RExpr)
            RSymbol name | not (T.isPrefixOf ":" name) -> do
                SomeRBody body <- pBodyRest []
                pure $ RExpr name body
            -- Во всех остальных случаях (число, строка, ключ) — это список данных
            _ -> do
                SomeRBody body <- pBodyRest [first]
                pure $ RList body

pBodyRest :: [Reactor] -> Parser SomeRBody
pBodyRest initial = do
    elems <- (initial ++) <$> many pReactor
    case elems of
        [] -> pure $ SomeRBody (RAtoms [])
        (x : _) | isProp x -> do
            props <- validateProps elems
            pure $ SomeRBody (RProps props)
        _ -> do
            validateNoProps elems
            pure $ SomeRBody (RAtoms elems)
  where
    isProp (RSymbol s) = T.isPrefixOf ":" s
    isProp _ = False

validateProps :: [Reactor] -> Parser [(Text, Reactor)]
validateProps = \case
    [] -> pure []
    [RSymbol k] | T.isPrefixOf ":" k -> customFailure (UnpairedProperty k)
    (RSymbol k : v : rest) | T.isPrefixOf ":" k -> do
        others <- validateProps rest
        pure ((T.drop 1 k, v) : others)
    (x : _) -> customFailure (MixedContent (T.pack $ show x))

validateNoProps :: [Reactor] -> Parser ()
validateNoProps = mapM_ \case
    RSymbol s | T.isPrefixOf ":" s -> customFailure (MixedContent s)
    _ -> pure ()
