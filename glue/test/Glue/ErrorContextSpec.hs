module Glue.ErrorContextSpec (spec) where

import Data.Text (Text)
import Glue.Env qualified as E
import Glue.Error (GlueError (..))
import Glue.Eval (Eval, eval, runEvalLegacy)
import Glue.Eval.Error (EvalError (..), GeneralError (..))
import Glue.IR (IR (..), compile)
import Glue.Lib (lib)
import Glue.Parser (parseGlue)
import Test.Hspec

runCode :: Text -> IO (Either GlueError (Maybe (IR Eval)))
runCode input = case parseGlue input of
    Left err -> pure $ Left (GlueError err)
    Right ast -> do
        let irTree = compile ast
        fullResult <- runEvalLegacy (eval irTree) (E.fromFrame lib)
        case fullResult of
            Left err -> pure $ Left (GlueError err)
            Right (res, _finalEnv, _ctx) -> pure $ Right res

spec :: Spec
spec = describe "Glue.ErrorContext (Call Stack Tracking)" do
    it "shows empty context for top-level errors" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (GlueError $ EvalError [] $ UnboundVariable "non-existent")

    it "shows context for direct function calls" do
        runCode "((lambda (a b) a) 1)"
            `shouldReturn` Left (GlueError $ EvalError ["<call>"] WrongNumberOfArguments)

    it "shows nested call stack" do
        runCode "((def f (lambda () (non-existent))) (f))"
            `shouldReturn` Left (GlueError $ EvalError ["f"] $ UnboundVariable "non-existent")

    it "shows deep call stack with multiple levels" do
        let code = "((def a (lambda () (b))) (def b (lambda () (non-existent))) (a))"
        runCode code
            `shouldReturn` Left (GlueError $ EvalError ["a"] $ UnboundVariable "b")

    it "shows context for user-defined functions" do
        runCode "((def id (lambda (x) x)) (id))"
            `shouldReturn` Left (GlueError $ EvalError ["id"] WrongNumberOfArguments)

    it "shows context for math operations" do
        runCode "(+ non-existent 1)"
            `shouldReturn` Left (GlueError $ EvalError ["+"] $ UnboundVariable "non-existent")

    it "shows context for property access errors" do
        runCode "((lambda (obj) obj.missing) (:x 42))"
            `shouldReturn` Left (GlueError $ EvalError ["<call>"] $ PropertyNotFound "missing")

    it "handles anonymous function calls" do
        runCode "((lambda () (non-existent)))"
            `shouldReturn` Left (GlueError $ EvalError ["<call>"] $ UnboundVariable "non-existent")
