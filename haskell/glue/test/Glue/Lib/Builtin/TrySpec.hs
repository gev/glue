module Glue.Lib.Builtin.TrySpec (spec) where

import Data.Text (Text)
import Glue.Env qualified as E
import Glue.Error (GlueError (..))
import Glue.Eval (Eval, eval, runEvalLegacy)
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
spec = describe "Glue.Lib.Builtin.Try (Test try special form)" do
    it "catches exception and calls handler with payload" do
        let code = "(try (error test-error (:msg \"hello\")) (catch \"test-error\" (lambda (err) err.msg)))"
        runCode code `shouldReturn` Right (Just (String "hello"))

    it "returns normal value when no exception" do
        let code = "(try 42 (catch \"any-error\" (lambda (err) \"caught\")))"
        runCode code `shouldReturn` Right (Just (Integer 42))

    it "re-throws unmatched exception" do
        let code = "(try (error test-error (:msg \"hello\")) (catch \"other-error\" (lambda (err) err.msg)))"
        result <- runCode code
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Expected error for unmatched exception"

    it "works with symbol catch names" do
        let code = "(try (error test-error (:msg \"hello\")) (catch test-error (lambda (err) err.msg)))"
        runCode code `shouldReturn` Right (Just (String "hello"))

    it "handler can be any callable" do
        let code = "(try (error test-error (:val 123)) (catch \"test-error\" (lambda (err) (+ err.val 1))))"
        runCode code `shouldReturn` Right (Just (Integer 124))

    it "multiple catch clauses work" do
        let code = "(try (error second-error (:msg \"second\")) (catch \"first-error\" (lambda (err) \"first\")) (catch \"second-error\" (lambda (err) err.msg)))"
        runCode code `shouldReturn` Right (Just (String "second"))

    it "first matching catch is used" do
        let code = "(try (error test-error (:msg \"caught\")) (catch \"test-error\" (lambda (err) err.msg)) (catch \"test-error\" (lambda (err) \"second\")))"
        runCode code `shouldReturn` Right (Just (String "caught"))

    -- Library function errors
    it "catches division by zero" do
        let code = "(try (/ 1 0) (catch \"div-by-zero\" (lambda () \"caught-div-zero\")))"
        runCode code `shouldReturn` Right (Just (String "caught-div-zero"))

    it "catches wrong argument type" do
        let code = "(try (+ \"string\" 1) (catch \"wrong-argument-type\" (lambda (err) \"caught-type-error\")))"
        runCode code `shouldReturn` Right (Just (String "caught-type-error"))

    it "catches unbound variable" do
        let code = "(try nonexistent-var (catch \"unbound-variable\" (lambda (err) \"caught-unbound\")))"
        runCode code `shouldReturn` Right (Just (String "caught-unbound"))

    -- Deep/nested errors
    it "catches errors in nested expressions" do
        let code = "(try (+ (/ 1 0) 42) (catch \"div-by-zero\" (lambda () \"nested-error\")))"
        runCode code `shouldReturn` Right (Just (String "nested-error"))

    it "catches errors in function calls" do
        let code = "(try ((lambda () (/ 1 0))) (catch \"div-by-zero\" (lambda () \"function-error\")))"
        runCode code `shouldReturn` Right (Just (String "function-error"))

    it "handles nested try blocks" do
        let code = "(try (try (/ 1 0) (catch \"div-by-zero\" (lambda () (error nested-error (:msg \"inner\"))))) (catch \"nested-error\" (lambda (err) err.msg)))"
        runCode code `shouldReturn` Right (Just (String "inner"))

    it "catches errors in complex expressions" do
        let code = "(try (+ (* 2 (/ 10 0)) 5) (catch \"div-by-zero\" (lambda () \"complex-error\")))"
        runCode code `shouldReturn` Right (Just (String "complex-error"))

    it "multiple error types in same try" do
        let code = "(try (if true (/ 1 0) (+ \"a\" 1)) (catch \"div-by-zero\" (lambda () \"div-error\")) (catch \"wrong-argument-type\" (lambda (err) \"type-error\")))"
        runCode code `shouldReturn` Right (Just (String "div-error"))

    it "error propagation through multiple function calls" do
        let code = "(try ((lambda (x) ((lambda (y) (/ y 0)) x)) 5) (catch \"div-by-zero\" (lambda () \"propagated-error\")))"
        runCode code `shouldReturn` Right (Just (String "propagated-error"))
