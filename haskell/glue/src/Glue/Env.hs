module Glue.Env where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (Env, Frame, IR (..))

emptyEnv :: Env m
emptyEnv = [Map.empty]

fromList :: [(Text, IR m)] -> Env m
fromList pairs = [Map.fromList pairs]

frameFromList :: [(Text, IR m)] -> Frame m
frameFromList = Map.fromList

fromFrame :: Frame m -> Env m
fromFrame = (: [])

pushFrame :: Env m -> Env m
pushFrame env = Map.empty : env

popFrame :: Env m -> Env m
popFrame (_ : fs) = fs
popFrame [] = []

lookupLocal :: Text -> Env m -> Maybe (IR m)
lookupLocal name (f : _) = Map.lookup name f
lookupLocal _ [] = Nothing

lookupVar :: Text -> Env m -> Either RuntimeException (IR m)
lookupVar name [] = Left $ UnboundVariable name
lookupVar name (f : fs) = case Map.lookup name f of
    Just val -> Right val
    Nothing -> lookupVar name fs

defineVar :: Text -> IR m -> Env m -> Env m
defineVar name val [] = [Map.singleton name val]
defineVar name val (f : fs) = Map.insert name val f : fs

updateVar :: Text -> IR m -> Env m -> Either RuntimeException (Env m)
updateVar name _ [] = Left $ CanNotSetUnboundVariable name
updateVar name val (f : fs)
    | Map.member name f = Right (Map.insert name val f : fs)
    | otherwise = (f :) <$> updateVar name val fs

unionFrames :: Frame m -> Frame m -> Frame m
unionFrames = Map.union

unionFramesList :: [Frame m] -> Frame m
unionFramesList = foldl unionFrames Map.empty
