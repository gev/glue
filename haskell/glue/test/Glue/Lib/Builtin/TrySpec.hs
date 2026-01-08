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

    -- Library function results (no longer errors)
    it "division by zero returns Infinity (not an error)" do
        let code = "(/ 1 0)"
        runCode code `shouldReturn` Right (Just (Float (1 / 0)))

    it "catches wrong argument type" do
        let code = "(try (+ \"string\" 1) (catch \"wrong-argument-type\" (lambda (err) \"caught-type-error\")))"
        runCode code `shouldReturn` Right (Just (String "caught-type-error"))

    it "catches unbound variable" do
        let code = "(try nonexistent-var (catch \"unbound-variable\" (lambda (err) \"caught-unbound\")))"
        runCode code `shouldReturn` Right (Just (String "caught-unbound"))

    -- Deep/nested expressions (division by zero now returns Infinity)
    it "handles Infinity in nested expressions" do
        let code = "(+ (/ 1 0) 42)"
        runCode code `shouldReturn` Right (Just (Float (1 / 0)))

    it "handles Infinity in function calls" do
        let code = "((lambda () (/ 1 0)))"
        runCode code `shouldReturn` Right (Just (Float (1 / 0)))

    it "handles nested expressions with Infinity" do
        let code = "(+ (* 2 (/ 10 0)) 5)"
        runCode code `shouldReturn` Right (Just (Float (1 / 0)))

    it "handles conditional with Infinity" do
        let code = "(if true (/ 1 0) (+ \"a\" 1))"
        runCode code `shouldReturn` Right (Just (Float (1 / 0)))

    it "handles Infinity in lambda expressions" do
        let code = "((lambda (x) ((lambda (y) (/ y 0)) x)) 5)"
        runCode code `shouldReturn` Right (Just (Float (1 / 0)))
