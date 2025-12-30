module Reactor.Lib.Builtin.ObjectSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Env qualified as E
import Reactor.Error (ReactorError (..))
import Reactor.Eval (Eval, eval, runEvalLegacy)
import Reactor.IR (IR (..), compile)
import Reactor.Lib (lib)
import Reactor.Parser (parseReactor)
import Test.Hspec

runCode :: Text -> IO (Either ReactorError (Maybe (IR Eval)))
runCode input = case parseReactor input of
    Left err -> pure $ Left (ReactorError err)
    Right ast -> do
        let irTree = compile ast
        fullResult <- runEvalLegacy (eval irTree) (E.fromFrame lib)
        case fullResult of
            Left err -> pure $ Left (ReactorError err)
            Right (res, _finalEnv, _ctx) -> pure $ Right res

spec :: Spec
spec = describe "Reactor.Lib.Builtin.Object (Integration tests)" do
    it "handles nested object property access" do
        let code = "(list (def foo (object :x (object :y (object :z 1)))) foo.x foo.x.y foo.x.y.z)"
        runCode code `shouldReturn` Right (Just (List [Object (Map.fromList [("y", Object (Map.fromList [("z", Number 1)]))]), Object (Map.fromList [("z", Number 1)]), Number 1]))
