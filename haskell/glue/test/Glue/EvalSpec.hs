module Glue.EvalSpec (spec) where

import Data.Map.Strict qualified as Map
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
spec = describe "Glue.Eval (System Integration)" do
    it "handles basic values" do
        runCode "42" `shouldReturn` Right (Just (Number 42))
        runCode "\"test\"" `shouldReturn` Right (Just (String "test"))

    it "executes (def) and (set) chain" do
        let code = "((def x 1) (set x 2) x)"
        runCode code `shouldReturn` Right (Just (List [Number 2]))

    it "implements full closures (Lexical Shadowing)" do
        let code = "(((lambda (x) (lambda (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Just (Number 100))

    it "checks that (def) inside (lambda) doesn't corrupt global scope" do
        let code = "((def x 1) ((lambda () (def x 2))) x)"
        runCode code `shouldReturn` Right (Just (List [Number 1]))

    it "handles property access on property lists" do
        let code = "((lambda (obj) obj.foo) (:foo 42))"
        runCode code `shouldReturn` Right (Just (Number 42))

    it "handles nested property access" do
        let code = "((def foo (:x (:y (:z 1)))) foo.x foo.x.y foo.x.y.z)"
        runCode code `shouldReturn` Right (Just (List [Object (Map.fromList [("y", Object (Map.fromList [("z", Number 1)]))]), Object (Map.fromList [("z", Number 1)]), Number 1]))

    it "fails when calling non-existent function" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (GlueError $ EvalError [] $ UnboundVariable "non-existent")

    it "fails when passing wrong number of arguments" do
        runCode "((lambda (a b) a) 1)"
            `shouldReturn` Left (GlueError $ EvalError ["<call>"] WrongNumberOfArguments)

    it "user-defined function" do
        runCode "((def id (lambda (x) x)) (id 42))"
            `shouldReturn` Right (Just (List [Number 42]))

    it "user-defined function too few args" do
        runCode "((def id (lambda (x) x)) (id))"
            `shouldReturn` Left (GlueError $ EvalError ["id"] WrongNumberOfArguments)

    it "user-defined function too many args" do
        runCode "((def id (lambda (x) x)) (id 1 2))"
            `shouldReturn` Left (GlueError $ EvalError ["id"] WrongNumberOfArguments)

    it "user-defined function multi-param" do
        runCode "((def f (lambda (a b) ((a) (b)))) (f 1 2))"
            `shouldReturn` Right (Just (List [List [Number 1, Number 2]]))

    it "\\ alias works like lambda (lexical shadowing)" do
        let code = "((( \\ (x) ( \\ (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Just (Number 100))

    it "\\ alias works like lambda (user-defined function)" do
        runCode "((def id (\\ (x) x)) (id 42))"
            `shouldReturn` Right (Just (List [Number 42]))

    it "\\ alias works like lambda (too few args)" do
        runCode "((def id (\\ (x) x)) (id))"
            `shouldReturn` Left (GlueError $ EvalError ["id"] WrongNumberOfArguments)

    it "\\ alias works like lambda (too many args)" do
        runCode "((def id (\\ (x) x)) (id 1 2))"
            `shouldReturn` Left (GlueError $ EvalError ["id"] WrongNumberOfArguments)

    it "\\ alias works like lambda (multi-param)" do
        runCode "((def f (\\ (a b) ((a) (b)))) (f 1 2))"
            `shouldReturn` Right (Just (List [List [Number 1, Number 2]]))

    it "== alias works like eq" do
        runCode "(== 42 42)" `shouldReturn` Right (Just (Bool True))
        runCode "(== 42 43)" `shouldReturn` Right (Just (Bool False))

    it "\\= alias works like ne" do
        runCode "(!= 42 43)" `shouldReturn` Right (Just (Bool True))
        runCode "(!= 42 42)" `shouldReturn` Right (Just (Bool False))

    it "< alias works like lt" do
        runCode "(< 5 10)" `shouldReturn` Right (Just (Bool True))
        runCode "(< 10 5)" `shouldReturn` Right (Just (Bool False))

    it "<= alias works like le" do
        runCode "(<= 5 5)" `shouldReturn` Right (Just (Bool True))
        runCode "(<= 10 5)" `shouldReturn` Right (Just (Bool False))

    it "> alias works like gt" do
        runCode "(> 10 5)" `shouldReturn` Right (Just (Bool True))
        runCode "(> 5 10)" `shouldReturn` Right (Just (Bool False))

    it ">= alias works like ge" do
        runCode "(>= 5 5)" `shouldReturn` Right (Just (Bool True))
        runCode "(>= 5 10)" `shouldReturn` Right (Just (Bool False))

    it "! alias works like not" do
        runCode "(! false)" `shouldReturn` Right (Just (Bool True))
        runCode "(! true)" `shouldReturn` Right (Just (Bool False))

    it "literal lists evaluate expressions" do
        runCode "((+ 1 2) (* 3 4))" `shouldReturn` Right (Just (List [Number 3, Number 12]))

    it "literal objects evaluate values" do
        runCode "(:x (+ 1 2) :y (* 3 4))" `shouldReturn` Right (Just (Object (Map.fromList [("x", Number 3), ("y", Number 12)])))

    it "dotted symbols work in function calls" do
        runCode "((def obj (:x (:y (:z (lambda (n) (+ n 10)))))) (obj.x.y.z 5))"
            `shouldReturn` Right (Just (List [Number 15]))
