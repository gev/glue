module Glue.Compile (
    compile,
) where

import Data.Bifunctor (second)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.AST (AST)
import Glue.AST qualified as AST
import Glue.IR (IR (..))

-- | Compile AST to IR
compile :: AST -> IR m
compile = \case
    AST.Integer n -> Integer n
    AST.Float n -> Float n
    AST.String s -> String s
    AST.Symbol s ->
        if T.isInfixOf "." s
            then DottedSymbol (T.splitOn "." s)
            else Symbol s
    AST.List xs -> List (map compile xs)
    AST.Object ps -> Object $ Map.fromList (map (second compile) ps)
