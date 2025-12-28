module Reactor.Lib.List.SortSpec (spec) where

import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib.List.Sort qualified as Sort
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.List.Sort (Test sort function)" do
    it "sorts a list of numbers in ascending order" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 3, Number 1, Number 4, Number 1, Number 5]]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 1, Number 3, Number 4, Number 5]

    it "sorts a list of strings in alphabetical order" do
        let initialEnv = E.emptyEnv
        let args = [List [String "zebra", String "apple", String "banana"]]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [String "apple", String "banana", String "zebra"]

    it "sorts a list of symbols in alphabetical order" do
        let initialEnv = E.emptyEnv
        let args = [List [Symbol "zebra", Symbol "apple", Symbol "banana"]]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Symbol "apple", Symbol "banana", Symbol "zebra"]

    it "sorts an empty list" do
        let initialEnv = E.emptyEnv
        let args = [List []]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "sorts a single element list" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 42]]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 42]

    it "sorts a list with duplicate elements" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 3, Number 1, Number 3, Number 1, Number 2]]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left err -> expectationFailure $ "Sort failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Number 1, Number 1, Number 2, Number 3, Number 3]

    it "fails on non-list argument" do
        let initialEnv = E.emptyEnv
        let args = [Number 42]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Sort should fail on non-list"

    it "fails on list with incomparable elements" do
        let initialEnv = E.emptyEnv
        let args = [List [Number 1, List [Number 2]]]
        result <- runEval (Sort.sort args) initialEnv
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Sort should fail on incomparable elements"
