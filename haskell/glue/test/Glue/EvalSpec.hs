module Glue.EvalSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Glue.Error (GlueError (..))
import Glue.Eval (Eval, eval, runEvalSimple)
import Glue.Eval.Error (EvalError (..))
import Glue.Eval.Exception
import Glue.IR (IR (..), compile)
import Glue.Lib.Bool (bool)
import Glue.Lib.Builtin (builtin)
import Glue.Lib.Math.Arithmetic (arithmetic)
import Glue.Module (envFromModules)
import Glue.Parser (parseGlue)
import Test.Hspec

runCode :: Text -> IO (Either GlueError (IR Eval))
runCode input = case parseGlue input of
    Left err -> pure $ Left (GlueError err)
    Right ast -> do
        let irTree = compile ast
        fullResult <- runEvalSimple (eval irTree) $ envFromModules [builtin, arithmetic, bool]
        case fullResult of
            Left err -> pure $ Left (GlueError err)
            Right (res, _finalEnv, _ctx) -> pure $ Right res

spec :: Spec
spec = describe "Glue.Eval (System Integration)" do
    it "handles basic values" do
        runCode "42" `shouldReturn` Right (Integer 42)
        runCode "\"test\"" `shouldReturn` Right (String "test")

    it "handles basic values" do
        runCode "(42)" `shouldReturn` Right (List [Integer 42])

    it "handles basic values" do
        runCode "((42))" `shouldReturn` Right (List [List [Integer 42]])

    it "handles basic values" do
        runCode "(+ 0 42)" `shouldReturn` Right (Integer 42)

    it "handles basic values" do
        runCode "((+ 0 42))" `shouldReturn` Right (List [Integer 42])

    it "handles basic values" do
        runCode "(== (+ 1 1) (+ 1 1))" `shouldReturn` Right (Bool True)

    it "handles basic values" do
        runCode "(== (+ 1 1) ((+ 1 1)))" `shouldReturn` Right (Bool False)

    it "executes (def)" do
        let code = "((def x 1) x)"
        runCode code `shouldReturn` Right (List [Void, Integer 1])

    it "should this work?" do
        let code = "((def x 1) (def y 2) (+ x y))"
        runCode code `shouldReturn` Right (List [Void, Void, Integer 3])

    it "executes (def)" do
        let code = "((def x (1)) x)"
        runCode code `shouldReturn` Right (List [Void, List [Integer 1]])

    it "executes (def)" do
        let code = "(1 ((def x 1) x))"
        runCode code `shouldReturn` Right (List [Integer 1, List [Void, Integer 1]])

    it "executes (def) and (set) chain" do
        let code = "((def x 1) (set x 2) x)"
        runCode code `shouldReturn` Right (List [Void, Void, Integer 2])

    it "implements full closures (Lexical Shadowing)" do
        let code = "(((lambda (x) (lambda (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Integer 100)

    it "checks that (def) inside (lambda) doesn't corrupt global scope" do
        let code = "((def x 1) ((lambda () (def x 2))) x)"
        runCode code `shouldReturn` Right (List [Void, Void, Integer 1])

    it "handles property access on property lists" do
        let code = "((lambda (obj) obj.foo) (:foo 42))"
        runCode code `shouldReturn` Right (Integer 42)

    it "handles nested property access" do
        let code = "((def foo (:x (:y (:z 1)))) foo.x foo.x.y foo.x.y.z)"
        runCode code `shouldReturn` Right (List [Void, Object (Map.fromList [("y", Object (Map.fromList [("z", Integer 1)]))]), Object (Map.fromList [("z", Integer 1)]), Integer 1])

    it "fails when calling non-existent function" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (GlueError (EvalError @Eval [] $ unboundVariable "non-existent"))

    it "partial application returns closure" do
        result <- runCode "((lambda (a b) a) 1)"
        result
            `shouldSatisfy` ( \case
                                Right (Closure ["b"] _ _) -> True
                                _ -> False
                            )

    it "user-defined function" do
        runCode "((def id (lambda (x) x)) (id 42))"
            `shouldReturn` Right (List [Void, Integer 42])

    it "user-defined function partial application (currying)" do
        result <- runCode "((def add (lambda (x y) (+ x y))) ((add 5) 3))"
        result `shouldBe` Right (List [Void, Integer 8])

    it "user-defined function returns closure on partial application" do
        result <- runCode "((def add (lambda (x y) (+ x y))) (add 5))"
        result
            `shouldSatisfy` ( \case
                                Right (List [Void, Closure ["y"] _ _]) -> True
                                _ -> False
                            )

    it "currying works with multiple levels" do
        result <- runCode "((def add (lambda (x y z) (+ x (+ y z)))) (((add 1) 2) 3))"
        result `shouldBe` Right (List [Void, Integer 6])

    it "user-defined function too many args still fails" do
        runCode "((def id (lambda (x) x)) (id 1 2))"
            `shouldReturn` Left (GlueError (EvalError @Eval ["id"] wrongNumberOfArguments))

    it "user-defined function multi-param" do
        runCode "((def f (lambda (a b) ((a) (b)))) (f 1 2))"
            `shouldReturn` Right (List [Void, Integer 2])

    it "\\ alias works like lambda (lexical shadowing)" do
        let code = "((( \\ (x) ( \\ (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Integer 100)

    it "\\ alias works like lambda (user-defined function)" do
        runCode "((def id (\\ (x) x)) (id 42))"
            `shouldReturn` Right (List [Void, Integer 42])

    it "\\ alias works like lambda (partial application)" do
        result <- runCode "((def add (\\ (x y) (+ x y))) (def add5 (add 5)) (add5 3))"
        result `shouldBe` Right (List [Void, Void, Integer 8])

    it "\\ alias works like lambda (too many args)" do
        runCode "((def id (\\ (x) x)) (id 1 2))"
            `shouldReturn` Left (GlueError (EvalError @Eval ["id"] wrongNumberOfArguments))

    it "\\ alias works like lambda (multi-param)" do
        runCode "((def f (\\ (a b) ((a) (b)))) (f 1 2))"
            `shouldReturn` Right (List [Void, Integer 2])

    it "\\ alias works like lambda (multi-param)" do
        runCode "((\\ (a b) ((a) (b))) 1 2)"
            `shouldReturn` Right (Integer 2)

    it "== alias works like eq" do
        runCode "(== 42 42)" `shouldReturn` Right (Bool True)
        runCode "(== 42 43)" `shouldReturn` Right (Bool False)

    it "\\= alias works like ne" do
        runCode "(!= 42 43)" `shouldReturn` Right (Bool True)
        runCode "(!= 42 42)" `shouldReturn` Right (Bool False)

    it "< alias works like lt" do
        runCode "(< 5 10)" `shouldReturn` Right (Bool True)
        runCode "(< 10 5)" `shouldReturn` Right (Bool False)

    it "<= alias works like le" do
        runCode "(<= 5 5)" `shouldReturn` Right (Bool True)
        runCode "(<= 10 5)" `shouldReturn` Right (Bool False)

    it "> alias works like gt" do
        runCode "(> 10 5)" `shouldReturn` Right (Bool True)
        runCode "(> 5 10)" `shouldReturn` Right (Bool False)

    it ">= alias works like ge" do
        runCode "(>= 5 5)" `shouldReturn` Right (Bool True)
        runCode "(>= 5 10)" `shouldReturn` Right (Bool False)

    it "! alias works like not" do
        runCode "(! false)" `shouldReturn` Right (Bool True)
        runCode "(! true)" `shouldReturn` Right (Bool False)

    it "literal lists evaluate expressions" do
        runCode "((+ 1 2) (* 3 4))" `shouldReturn` Right (List [Integer 3, Integer 12])

    it "literal objects evaluate values" do
        runCode "(:x (+ 1 2) :y (* 3 4))" `shouldReturn` Right (Object (Map.fromList [("x", Integer 3), ("y", Integer 12)]))

    it "dotted symbols work in function calls" do
        runCode "((def obj (:x (:y (:z (lambda (n) (+ n 10)))))) (obj.x.y.z 5))"
            `shouldReturn` Right (List [Void, Integer 15])

    it "deep arithmetic composition" do
        runCode "(* (+ 1 2) (- 10 2))"
            `shouldReturn` Right (Integer 24)

    it "complex arithmetic with mixed operations" do
        runCode "(/ (+ (* 3 4) 2) (- 10 3))"
            `shouldReturn` Right (Float 2.0)

    it "deep arithmetic with floats" do
        runCode "(+ (* 2.5 4.0) (/ 10.0 2.0))"
            `shouldReturn` Right (Float 15.0)

    it "let creates local bindings" do
        runCode "(let (:x 42) x)"
            `shouldReturn` Right (Integer 42)

    it "let bindings can access outer scope" do
        runCode "((def outer 100) (let (:x outer) (+ x 1)))"
            `shouldReturn` Right (List [Void, Integer 101])

    it "let bindings shadow outer scope" do
        runCode "((def x 100) (let (:x 200) x))"
            `shouldReturn` Right (List [Void, Integer 200])

    it "let with multiple bindings" do
        runCode "(let (:x 10 :y 20) (+ x y))"
            `shouldReturn` Right (Integer 30)

    it "let bindings are local" do
        runCode "((let (:x 42) x) x)"
            `shouldReturn` Left (GlueError (EvalError @Eval [] $ unboundVariable "x"))

    it "arithmetic with defined functions" do
        runCode "((def add (lambda (x y) (+ x y))) (def mul (lambda (x y) (* x y))) (mul (add 3 2) (add 1 2)))"
            `shouldReturn` Right (List [Void, Void, Integer 15])

    it "nested function calls with arithmetic" do
        runCode "((def calc (lambda (a b) (* (+ a b) (- a b)))) (calc 5 3))"
            `shouldReturn` Right (List [Void, Integer 16])

    it "function bodies with lists return last value (implicit sequences)" do
        -- Test direct lambda call first
        runCode "((\\ (x y) (42 (+ x y))) 1 2)"
            `shouldReturn` Right (Integer 3)

    it "function bodies with direct expressions work" do
        runCode "((\\ (x y) x) 1 2)"
            `shouldReturn` Right (Integer 1)

    it "function bodies with single-element lists work" do
        runCode "((\\ (x y) ((+ x y))) 1 2)"
            `shouldReturn` Right (Integer 3)
