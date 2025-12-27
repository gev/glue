module Reactor.Lib.Arithmetic.ModSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import qualified Reactor.Lib.Arithmetic.Mod as Mod
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Arithmetic.Mod (Test mod function)" do
    describe "Mod function" do
        it "returns 1 for (% 7 3)" do
            let args = [Number 7, Number 3]
            result <- runEval (Mod.mod args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mod failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 1

        it "returns 0 for (% 6 3)" do
            let args = [Number 6, Number 3]
            result <- runEval (Mod.mod args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mod failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 0

        it "returns 2 for (% 17 5)" do
            let args = [Number 17, Number 5]
            result <- runEval (Mod.mod args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Mod failed: " <> show err
                Right (res, _) -> res `shouldBe` Number 2

        it "fails with wrong number of arguments" do
            let args = [Number 7]
            result <- runEval (Mod.mod args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with division by zero" do
            let args = [Number 7, Number 0]
            result <- runEval (Mod.mod args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft

        it "fails with non-numbers" do
            let args = [Number 7, String "hello"]
            result <- runEval (Mod.mod args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
