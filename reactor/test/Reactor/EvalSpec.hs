module Reactor.EvalSpec (spec) where

import Data.Text (Text)
import Reactor.Env qualified as E
import Reactor.Error (ReactorError (..))
import Reactor.Eval (Eval, eval, runEvalLegacy)
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
        fullResult <- runEvalLegacy (eval irTree) (E.fromFrame lib)
        case fullResult of
            Left err -> pure $ Left (ReactorError err)
            Right (res, _finalEnv, _ctx) -> pure $ Right res

spec :: Spec
spec = describe "Reactor.Eval (System Integration)" do
    it "handles basic values" do
        runCode "42" `shouldReturn` Right (Just (Number 42))
        runCode "\"test\"" `shouldReturn` Right (Just (String "test"))

    it "executes (def) and (set) chain" do
        let code = "(list (def x 1) (set x 2) x)"
        runCode code `shouldReturn` Right (Just (List [Number 2]))

    it "implements full closures (Lexical Shadowing)" do
        let code = "(((lambda (x) (lambda (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Just (Number 100))

    it "checks that (def) inside (lambda) doesn't corrupt global scope" do
        let code = "(list (def x 1) ((lambda () (def x 2))) x)"
        runCode code `shouldReturn` Right (Just (List [Number 1]))

    it "handles property access on property lists" do
        let code = "(:foo 42).foo"
        runCode code `shouldReturn` Right (Just (Number 42))

    it "fails when calling non-existent function" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (ReactorError $ EvalError [] $ UnboundVariable "non-existent")

    it "fails when passing wrong number of arguments" do
        runCode "((lambda (a b) a) 1)"
            `shouldReturn` Left (ReactorError $ EvalError ["<call>"] WrongNumberOfArguments)

    it "user-defined function" do
        runCode "(list (def id (lambda (x) x)) (id 42))"
            `shouldReturn` Right (Just (List [Number 42]))

    it "user-defined function too few args" do
        runCode "(list (def id (lambda (x) x)) (id))"
            `shouldReturn` Left (ReactorError $ EvalError ["id", "list"] WrongNumberOfArguments)

    it "user-defined function too many args" do
        runCode "(list (def id (lambda (x) x)) (id 1 2))"
            `shouldReturn` Left (ReactorError $ EvalError ["id", "list"] WrongNumberOfArguments)

    it "user-defined function multi-param" do
        runCode "(list (def f (lambda (a b) (list a b))) (f 1 2))"
            `shouldReturn` Right (Just (List [List [Number 1, Number 2]]))

    it "\\ alias works like lambda (lexical shadowing)" do
        let code = "((( \\ (x) ( \\ (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Just (Number 100))

    it "\\ alias works like lambda (user-defined function)" do
        runCode "(list (def id (\\ (x) x)) (id 42))"
            `shouldReturn` Right (Just (List [Number 42]))

    it "\\ alias works like lambda (too few args)" do
        runCode "(list (def id (\\ (x) x)) (id))"
            `shouldReturn` Left (ReactorError $ EvalError ["id", "list"] WrongNumberOfArguments)

    it "\\ alias works like lambda (too many args)" do
        runCode "(list (def id (\\ (x) x)) (id 1 2))"
            `shouldReturn` Left (ReactorError $ EvalError ["id", "list"] WrongNumberOfArguments)

    it "\\ alias works like lambda (multi-param)" do
        runCode "(list (def f (\\ (a b) (list a b))) (f 1 2))"
            `shouldReturn` Right (Just (List [List [Number 1, Number 2]]))

    it "== alias works like eq" do
        runCode "(== 42 42)" `shouldReturn` Right (Just (Symbol "true"))
        runCode "(== 42 43)" `shouldReturn` Right (Just (Symbol "false"))

    it "\\= alias works like ne" do
        runCode "(\\= 42 43)" `shouldReturn` Right (Just (Symbol "true"))
        runCode "(\\= 42 42)" `shouldReturn` Right (Just (Symbol "false"))

    it "< alias works like lt" do
        runCode "(< 5 10)" `shouldReturn` Right (Just (Symbol "true"))
        runCode "(< 10 5)" `shouldReturn` Right (Just (Symbol "false"))

    it "<= alias works like le" do
        runCode "(<= 5 5)" `shouldReturn` Right (Just (Symbol "true"))
        runCode "(<= 10 5)" `shouldReturn` Right (Just (Symbol "false"))

    it "> alias works like gt" do
        runCode "(> 10 5)" `shouldReturn` Right (Just (Symbol "true"))
        runCode "(> 5 10)" `shouldReturn` Right (Just (Symbol "false"))

    it ">= alias works like ge" do
        runCode "(>= 5 5)" `shouldReturn` Right (Just (Symbol "true"))
        runCode "(>= 5 10)" `shouldReturn` Right (Just (Symbol "false"))

    it "! alias works like not" do
        runCode "(! false)" `shouldReturn` Right (Just (Symbol "true"))
        runCode "(! true)" `shouldReturn` Right (Just (Symbol "false"))
