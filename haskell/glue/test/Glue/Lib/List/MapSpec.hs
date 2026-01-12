module Glue.Lib.List.MapSpec (spec) where

import Glue.Eval (runEvalSimple)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Map qualified as Map
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.List.Map (Test map function)" do
    it "maps a function over a list of numbers" do
        let func = Native (Func (\[Integer x] -> pure $ Float (fromIntegral x * 2)))
        let args = [func, List [Integer 1, Integer 2, Integer 3]]
        result <- runEvalSimple (Map.map args) []
        case result of
            Left err -> expectationFailure $ "Map failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List [Float 2.0, Float 4.0, Float 6.0]

    it "maps over empty list" do
        let func = Native (Func (\[Integer x] -> pure $ Float (fromIntegral x + 1)))
        let args = [func, List []]
        result <- runEvalSimple (Map.map args) []
        case result of
            Left err -> expectationFailure $ "Map failed: " <> show err
            Right (res, _, _) -> res `shouldBe` List []

    it "fails on non-list second argument" do
        let func = Native (Func (\[Integer x] -> pure $ Float (fromIntegral x + 1)))
        let args = [func, Integer 42]
        result <- runEvalSimple (Map.map args) []
        case result of
            Left _ -> pure () -- Expected error
            Right _ -> expectationFailure "Map should fail on non-list"
