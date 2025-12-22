module Reactor.Parser (
    parseReactor,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Reactor.AST
import Reactor.Error (ReactorError (..), renderParserError)

type Parser = Parsec ReactorError Text

-- | Главная точка входа
parseReactor :: Text -> Either ReactorError Reactor
parseReactor input =
    case parse (pReactor <* eof) "reactor-input" input of
        Left err -> Left (renderParserError err)
        Right ast -> Right ast

-- --- Основные парсеры ---

pReactor :: Parser Reactor
pReactor =
    choice
        [ pQuoted -- Сахар должен быть перед остальными!
        , pExprOrList
        , pString
        , pNumber
        , pSymbol
        ]

-- | Реализация цитирования: 'something -> (quote something)
pQuoted :: Parser Reactor
pQuoted = do
    _ <- char '\''
    inner <- pReactor -- рекурсивно парсим то, что идет за кавычкой
    -- Генерируем AST эквивалентное (quote ...)
    pure $ RExpr "quote" (RAtoms [inner])

-- --- Атомы и лексер ---

pNumber :: Parser Reactor
pNumber = RNumber <$> lexeme L.scientific

pString :: Parser Reactor
pString = RString . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

pSymbol :: Parser Reactor
pSymbol = do
    s <- lexeme $ T.pack <$> some (alphaNumChar <|> oneOf ("-._:!?" :: String))
    pure $ RSymbol s

-- --- Структуры ( ... ) ---

pExprOrList :: Parser Reactor
pExprOrList = between (symbol "(") (symbol ")") do
    lookAhead (optional anySingle) >>= \case
        Nothing -> pure $ RList (RAtoms [])
        Just _ -> do
            first <- pReactor
            case first of
                -- Если первый элемент - символ и не ключ, это выражение (RExpr)
                RSymbol name | not (T.isPrefixOf ":" name) -> do
                    if name == "error"
                        then customFailure (ReservedKeyword "error")
                        else do
                            atoms <- many pReactor
                            -- Мы упростили здесь: pBodyRest можно встроить или вызвать
                            pure $ RExpr name (RAtoms (first : atoms))
                _ -> do
                    -- Логика парсинга свойств (RProps) или атомов (RAtoms)
                    -- (используйте вашу существующую pBodyRest)
                    undefined

-- --- Хелперы лексера ---
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
