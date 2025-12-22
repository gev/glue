{-# LANGUAGE ViewPatterns #-}

module Reactor.Error where

import Data.List.NonEmpty (head)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (ParseErrorBundle (bundleErrors))
import Text.Megaparsec.Error (ErrorFancy (..), ParseError (..), ShowErrorComponent (..), errorBundlePretty)
import Prelude hiding (head)

data ReactorError
    = MixedContent Text -- Имя ключа, который нарушил структуру
    | UnpairedProperty Text -- Ключ, которому не хватило значения
    | ReservedKeyword Text -- Использование зарезервированного слова не по назначению
    | SyntaxError Text
    deriving (Eq, Show, Ord)

-- Реализуем инстанс для красивого вывода
instance ShowErrorComponent ReactorError where
    showErrorComponent :: ReactorError -> String
    showErrorComponent = \case
        MixedContent k ->
            "Syntax Error: Property '"
                ++ T.unpack k
                ++ "' cannot be mixed with positional arguments.\n"
                ++ "In Reactor LISP, a list must be EITHER all properties (:key val) OR all atoms."
        UnpairedProperty k ->
            "Syntax Error: The property '" ++ T.unpack k ++ "' is missing a value."
        ReservedKeyword s ->
            "Syntax Error: '" ++ T.unpack s ++ "' is a reserved keyword."
        SyntaxError e ->
            "Syntax Error: '" ++ T.unpack e

-- Эта функция "схлопывает" Megaparsec Bundle в один ReactorError
renderParserError :: ParseErrorBundle Text ReactorError -> ReactorError
renderParserError bundle =
    case head (bundleErrors bundle) of
        -- Если это наша кастомная ошибка через customFailure
        FancyError _ (Set.toList -> [ErrorCustom e]) -> e
        -- Во всех остальных случаях (системные ошибки) берем красивый текст Megaparsec
        _ -> SyntaxError (T.pack $ errorBundlePretty bundle)
