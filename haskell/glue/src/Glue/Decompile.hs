module Glue.Decompile (
  decompile,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.AST (AST)
import Glue.AST qualified as AST
import Glue.IR (IR (..))

{- | Decompile IR to AST (reverse of compile)
Returns Left error for non-serializable IR types
-}
decompile :: IR m -> Either String AST
decompile = \case
  Integer n -> Right (AST.Integer n)
  Float n -> Right (AST.Float n)
  String s -> Right (AST.String s)
  Bool b -> Right (AST.Symbol (if b then "true" else "false"))
  Symbol s -> Right (AST.Symbol s)
  DottedSymbol parts -> Right (AST.Symbol (T.intercalate "." parts))
  List xs -> AST.List <$> mapM decompile xs
  Object ps -> AST.Object <$> mapM decompilePair (Map.toAscList ps)
  Void -> Left "Cannot decompile Void"
  Evaluable _ -> Left "Cannot decompile Evaluable"
  NativeValue _ -> Left "Cannot decompile NativeValue"
  NativeFunc _ -> Left "Cannot decompile NativeFunc"
  Special _ -> Left "Cannot decompile Special"
  Closure _ _ _ -> Left "Cannot decompile Closure"
 where
  decompilePair :: (T.Text, IR m) -> Either String (T.Text, AST)
  decompilePair (k, v) = case decompile v of
    Left err -> Left err
    Right ast -> Right (k, ast)
