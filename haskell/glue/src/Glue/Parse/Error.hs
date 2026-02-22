{-# LANGUAGE ViewPatterns #-}

module Glue.Parse.Error where

import Data.List.NonEmpty (head)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec qualified as M
import Prelude hiding (head)

data ParseError
    = MixedContent Text
    | UnpairedProperty Text
    | SyntaxError Text
    deriving (Eq, Show, Ord)

instance M.ShowErrorComponent ParseError where
    showErrorComponent = \case
        MixedContent k ->
            "Syntax Error: Property '"
                <> T.unpack k
                <> "' cannot be mixed with positional arguments.\n"
                <> "In Glue LISP, a list must be EITHER all properties (:key val) OR all atoms."
        UnpairedProperty k ->
            "Syntax Error: The property '" <> T.unpack k <> "' is missing a value."
        SyntaxError e ->
            "Syntax Error: '" <> T.unpack e

parserError :: M.ParseErrorBundle Text ParseError -> ParseError
parserError bundle =
    case head (M.bundleErrors bundle) of
        M.FancyError _ (Set.toList -> [M.ErrorCustom e]) -> e
        _ -> SyntaxError (T.pack $ M.errorBundlePretty bundle)
