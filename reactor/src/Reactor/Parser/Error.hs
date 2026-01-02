{-# LANGUAGE ViewPatterns #-}

module Reactor.Parser.Error where

import Data.List.NonEmpty (head)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (ParseErrorBundle (bundleErrors))
import Text.Megaparsec.Error (ErrorFancy (..), ParseError (..), ShowErrorComponent (..), errorBundlePretty)
import Prelude hiding (head)

data ParserError
    = MixedContent Text
    | UnpairedProperty Text
    | SyntaxError Text
    deriving (Eq, Show, Ord)

instance ShowErrorComponent ParserError where
    showErrorComponent = \case
        MixedContent k ->
            "Syntax Error: Property '"
                <> T.unpack k
                <> "' cannot be mixed with positional arguments.\n"
                <> "In Reactor LISP, a list must be EITHER all properties (:key val) OR all atoms."
        UnpairedProperty k ->
            "Syntax Error: The property '" <> T.unpack k <> "' is missing a value."
        SyntaxError e ->
            "Syntax Error: '" <> T.unpack e

parserError :: ParseErrorBundle Text ParserError -> ParserError
parserError bundle =
    case head (bundleErrors bundle) of
        FancyError _ (Set.toList -> [ErrorCustom e]) -> e
        _ -> SyntaxError (T.pack $ errorBundlePretty bundle)
