module Glue.Lib.List.ZipSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.List.Zip qualified as Zip
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Zip (Test zip function)" do
    it "zips two lists of equal length" do
        let args = [List [Integer 1, Integer 2, Integer 3], List [String "a", String "b", String "c"]]
        result <- runEvalSimple (Zip.zip args) []
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Integer 1, String "a"], List [Integer 2, String "b"], List [Integer 3, String "c"]]

    it "zips two lists where first is shorter" do
        let args = [List [Integer 1, Integer 2], List [String "a", String "b", String "c"]]
        result <- runEvalSimple (Zip.zip args) []
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Integer 1, String "a"], List [Integer 2, String "b"]]

    it "zips two lists where second is shorter" do
        let args = [List [Integer 1, Integer 2, Integer 3], List [String "a", String "b"]]
        result <- runEvalSimple (Zip.zip args) []
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Integer 1, String "a"], List [Integer 2, String "b"]]

    it "zips two empty lists" do
        let args = [List [], List []]
        result <- runEvalSimple (Zip.zip args) []
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "zips one empty list with non-empty" do
        let args = [List [], List [Integer 1, Integer 2]]
        result <- runEvalSimple (Zip.zip args) []
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list first argument" do
        let args = [Integer 42, List [Integer 1, Integer 2]]
        result <- runEvalSimple (Zip.zip args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Zip should fail on non-list first argument"

    it "fails on non-list second argument" do
        let args = [List [Integer 1, Integer 2], Integer 42]
        result <- runEvalSimple (Zip.zip args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Zip should fail on non-list second argument"
