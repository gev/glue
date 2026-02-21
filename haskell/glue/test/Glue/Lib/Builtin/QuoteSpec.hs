module Glue.Lib.Builtin.QuoteSpec (spec) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Builtin.Quote (quote)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Builtin.Quote (Test quote special form)" do
    describe "Quote returns argument unevaluated" do
        it "returns integer unevaluated" do
            let args = [Integer 42]
            result <- runEvalSimple (apply quote args) []
            case result of
                Left err -> expectationFailure $ "Quote failed: " <> show err
                Right (res, _) -> res `shouldBe` Integer 42

        it "returns symbol unevaluated (not variable lookup)" do
            let args = [Symbol "x"]
            result <- runEvalSimple (apply quote args) []
            case result of
                Left err -> expectationFailure $ "Quote failed: " <> show err
                Right (res, _) -> res `shouldBe` Symbol "x"

        it "returns string unevaluated" do
            let args = [String "hello"]
            result <- runEvalSimple (apply quote args) []
            case result of
                Left err -> expectationFailure $ "Quote failed: " <> show err
                Right (res, _) -> res `shouldBe` String "hello"

        it "returns list unevaluated (not evaluated)" do
            let args = [List [Symbol "+", Integer 1, Integer 2]]
            result <- runEvalSimple (apply quote args) []
            case result of
                Left err -> expectationFailure $ "Quote failed: " <> show err
                Right (res, _) -> res `shouldBe` List [Symbol "+", Integer 1, Integer 2]

        it "returns object unevaluated" do
            let args = [Object (Map.fromList [("name", String "Alice")])]
            result <- runEvalSimple (apply quote args) []
            case result of
                Left err -> expectationFailure $ "Quote failed: " <> show err
                Right (res, _) -> res `shouldBe` Object (Map.fromList [("name", String "Alice")])

    describe "Error cases" do
        it "fails with no arguments" do
            let args = []
            result <- runEvalSimple (apply quote args) []
            result `shouldSatisfy` isLeft

        it "fails with multiple arguments" do
            let args = [Integer 1, Integer 2]
            result <- runEvalSimple (apply quote args) []
            result `shouldSatisfy` isLeft
