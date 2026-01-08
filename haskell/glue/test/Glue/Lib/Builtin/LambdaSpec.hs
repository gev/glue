module Glue.Lib.Builtin.LambdaSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.Builtin.Lambda (extractSymbols, lambda)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Builtin.Lambda (Test lambda special form)" do
    describe "Creating closures" do
        it "creates a closure with parameters and body" do
            let initialEnv = E.fromList [("x", Integer 10)]
            let args = [List [Symbol "a", Symbol "b"], Symbol "body"]
            result <- runEvalLegacy (lambda args) initialEnv
            case result of
                Left err -> expectationFailure $ "Lambda failed: " <> show err
                Right (res, _, _) -> case res of
                    Just (Closure params body capturedEnv) -> do
                        params `shouldBe` ["a", "b"]
                        body `shouldBe` Symbol "body"
                        -- capturedEnv should be the initialEnv
                        E.lookupVar "x" capturedEnv `shouldBe` Right (Integer 10)
                    _ -> expectationFailure "Expected Just Closure"

        it "creates a closure with no parameters" do
            let initialEnv = E.emptyEnv
            let args = [List [], Integer 42]
            result <- runEvalLegacy (lambda args) initialEnv
            case result of
                Left err -> expectationFailure $ "Lambda failed: " <> show err
                Right (res, _, _) -> case res of
                    Just (Closure params body _) -> do
                        params `shouldBe` []
                        body `shouldBe` Integer 42
                    _ -> expectationFailure "Expected Just Closure"

    describe "extractSymbols" do
        it "extracts symbols from list" do
            let irs = [Symbol "a", Symbol "b"]
            extractSymbols irs `shouldBe` Right ["a", "b"]

        it "fails on non-symbols" do
            let irs = [Symbol "a", Integer 1]
            extractSymbols irs `shouldSatisfy` isLeft

    describe "Error cases" do
        it "fails with wrong number of arguments" do
            let initialEnv = E.emptyEnv
            let args = [List [Symbol "x"]]
            result <- runEvalLegacy (lambda args) initialEnv
            result `shouldSatisfy` isLeft

        it "fails with non-list as parameters" do
            let initialEnv = E.emptyEnv
            let args = [Integer 1, Symbol "body"]
            result <- runEvalLegacy (lambda args) initialEnv
            result `shouldSatisfy` isLeft

        it "fails with non-symbols in parameters" do
            let initialEnv = E.emptyEnv
            let args = [List [Integer 1], Symbol "body"]
            result <- runEvalLegacy (lambda args) initialEnv
            result `shouldSatisfy` isLeft
