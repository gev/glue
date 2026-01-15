module Glue.Lib.Builtin.TrySpec (spec) where

import Data.Text (Text)
import Glue.Error (GlueError (..))
import Glue.Eval (Eval, eval, runEvalSimple)
import Glue.IR (IR (..), compile)
import Glue.Lib.Builtin (builtin)
import Glue.Lib.Math.Arithmetic (arithmetic)
import Glue.Module (envFromModules)
import Glue.Parser (parseGlue)
import Test.Hspec

runCode :: Text -> IO (Either GlueError (Maybe (IR Eval)))
runCode input = case parseGlue input of
    Left err -> pure $ Left (GlueError err)
    Right ast -> do
        let irTree = compile ast
        fullResult <- runEvalSimple (eval irTree) $ envFromModules [builtin, arithmetic]
        case fullResult of
            Left err -> pure $ Left (GlueError err)
            Right (res, _) -> pure $ Right (Just res)

spec :: Spec
spec = describe "Glue.Lib.Builtin.Try (Test try special form)" do
    it "catches exception and calls handler with payload" do
        let code = "(try (error test-error (:msg \"hello\")) (catch test-error (lambda (err) err.msg)))"
        runCode code `shouldReturn` Right (Just (String "hello"))

    it "returns normal value when no exception" do
        let code = "(try 42 (catch any-error (lambda (err) \"caught\")))"
        runCode code `shouldReturn` Right (Just (Integer 42))

    it "re-throws unmatched exception" do
        let code = "(try (error test-error (:msg \"hello\")) (catch other-error (lambda (err) err.msg)))"
        result <- runCode code
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Expected error for unmatched exception"

    it "works with symbol catch names" do
        let code = "(try (error test-error (:msg \"hello\")) (catch test-error (lambda (err) err.msg)))"
        runCode code `shouldReturn` Right (Just (String "hello"))

    it "handler can be any callable" do
        let code = "(try (error test-error (:val 123)) (catch test-error (lambda (err) (+ err.val 1))))"
        runCode code `shouldReturn` Right (Just (Integer 124))

    it "multiple catch clauses work" do
        let code = "(try (error second-error (:msg \"second\")) (catch first-error (lambda (err) \"first\")) (catch second-error (lambda (err) err.msg)))"
        runCode code `shouldReturn` Right (Just (String "second"))

    it "first matching catch is used" do
        let code = "(try (error test-error (:msg \"caught\")) (catch test-error (lambda (err) err.msg)) (catch test-error (lambda (err) \"second\")))"
        runCode code `shouldReturn` Right (Just (String "caught"))

    it "fails to catch when using string instead of symbol" do
        let code = "(try (error test-error (:msg \"hello\")) (catch \"test-error\" (lambda (err) err.msg)))"
        result <- runCode code
        case result of
            Left _ -> pure () -- Expected error since string can't match symbol
            Right _ -> expectationFailure "Expected error when using string in catch clause"
