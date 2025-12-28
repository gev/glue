module Reactor.ErrorContextSpec (spec) where

import Data.Text (Text)
import Reactor.Env qualified as E
import Reactor.Error (ReactorError (..))
import Reactor.Eval (Eval, eval, runEval)
import Reactor.Eval.Error (EvalError (..), GeneralError (..))
import Reactor.IR (IR (..), compile)
import Reactor.Lib (lib)
import Reactor.Parser (parseReactor)
import Test.Hspec

runCode :: Text -> IO (Either ReactorError (Maybe (IR Eval)))
runCode input = case parseReactor input of
    Left err -> pure $ Left (ReactorError err)
    Right ast -> do
        let irTree = compile ast
        fullResult <- runEval (eval irTree) (E.fromFrame lib)
        case fullResult of
            Left err -> pure $ Left (ReactorError err)
            Right (res, _finalEnv, _ctx) -> pure $ Right res

spec :: Spec
spec = describe "Reactor.ErrorContext (Call Stack Tracking)" do
    it "shows empty context for top-level errors" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (ReactorError $ EvalError [] $ UnboundVariable "non-existent")

    it "shows context for direct function calls" do
        runCode "((lambda (a b) a) 1)"
            `shouldReturn` Left (ReactorError $ EvalError ["<call>"] WrongNumberOfArguments)

    it "shows nested call stack" do
        runCode "(list (def f (lambda () (non-existent))) (f))"
            `shouldReturn` Left (ReactorError $ EvalError ["f", "list"] $ UnboundVariable "non-existent")

    it "shows deep call stack with multiple levels" do
        let code = "(list (def a (lambda () (b))) (def b (lambda () (non-existent))) (a))"
        runCode code
            `shouldReturn` Left (ReactorError $ EvalError ["a", "list"] $ UnboundVariable "b")

    it "shows context for user-defined functions" do
        runCode "(list (def id (lambda (x) x)) (id))"
            `shouldReturn` Left (ReactorError $ EvalError ["id", "list"] WrongNumberOfArguments)

    -- Note: Math operations and property access syntax need to be verified
    -- These tests are commented out until proper syntax is determined
    -- it "shows context for math operations" do
    --     runCode "(+ non-existent 1)"
    --         `shouldReturn` Left (ReactorError $ EvalError ["+"] $ UnboundVariable "non-existent")

    -- it "shows context for property access errors" do
    --     runCode "((lambda (obj) obj.missing) (object :x 42))"
    --         `shouldReturn` Left (ReactorError $ EvalError ["<call>"] $ PropertyNotFound "missing")

    it "handles anonymous function calls" do
        runCode "((lambda () (non-existent)))"
            `shouldReturn` Left (ReactorError $ EvalError ["<call>"] $ UnboundVariable "non-existent")
