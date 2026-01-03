module Glue.Lib.List.ZipSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (runEvalLegacy)
import Glue.IR (IR (..))
import Glue.Lib.List.Zip qualified as Zip
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Zip (Test zip function)" do
    it "zips two lists of equal length" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2, Number 3], List [String "a", String "b", String "c"]]
        result <- runEvalLegacy (Zip.zip args) initialEnv
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Number 1, String "a"], List [Number 2, String "b"], List [Number 3, String "c"]]

    it "zips two lists where first is shorter" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2], List [String "a", String "b", String "c"]]
        result <- runEvalLegacy (Zip.zip args) initialEnv
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Number 1, String "a"], List [Number 2, String "b"]]

    it "zips two lists where second is shorter" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2, Number 3], List [String "a", String "b"]]
        result <- runEvalLegacy (Zip.zip args) initialEnv
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [List [Number 1, String "a"], List [Number 2, String "b"]]

    it "zips two empty lists" do
        let initialEnv = E.emptyEnv
        let args = [List [], List []]
        result <- runEvalLegacy (Zip.zip args) initialEnv
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "zips one empty list with non-empty" do
        let initialEnv = E.emptyEnv
        let args = [List [], List [Number 1, Number 2]]
        result <- runEvalLegacy (Zip.zip args) initialEnv
        case result of
            Left err -> expectationFailure $ "Zip failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list first argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 42, List [Number 1, Number 2]]
        result <- runEvalLegacy (Zip.zip args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Zip should fail on non-list first argument"

    it "fails on non-list second argument" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, Number 2], Number 42]
        result <- runEvalLegacy (Zip.zip args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Zip should fail on non-list second argument"
