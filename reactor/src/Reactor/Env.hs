module Reactor.Env where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (Env, IR (..))

emptyEnv :: Env m
emptyEnv = [Map.empty]

fromList :: [(Text, IR m)] -> Env m
fromList pairs = [Map.fromList pairs]

pushFrame :: Env m -> Env m
pushFrame env = Map.empty : env

popFrame :: Env m -> Env m
popFrame (_ : fs) = fs
popFrame [] = []

lookupLocal :: Text -> Env m -> Maybe (IR m)
lookupLocal name (f : _) = Map.lookup name f
lookupLocal _ [] = Nothing

lookupVar :: Text -> Env m -> Either EvalError (IR m)
lookupVar name [] = Left $ UnboundVariable name
lookupVar name (f : fs) = case Map.lookup name f of
    Just val -> Right val
    Nothing -> lookupVar name fs

defineVar :: Text -> IR m -> Env m -> Env m
defineVar name val [] = [Map.singleton name val]
defineVar name val (f : fs) = Map.insert name val f : fs

updateVar :: Text -> IR m -> Env m -> Either EvalError (Env m)
updateVar name _ [] = Left $ CanNotSetUnboundVariable name
updateVar name val (f : fs)
    | Map.member name f = Right (Map.insert name val f : fs)
    | otherwise = (f :) <$> updateVar name val fs
