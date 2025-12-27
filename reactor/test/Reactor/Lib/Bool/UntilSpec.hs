module Reactor.Lib.Bool.UntilSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEval)
import Reactor.IR (IR (..))
import Reactor.Lib (lib)
import Reactor.Lib.Bool.Until (until_)
import Test.Hspec

spec :: Spec
spec = describe "Reactor.Lib.Bool.Until (Test until special form)" do
    describe "Loop until condition" do
        it "returns nothing when condition is true and no body" do
            let args = [Symbol "true"] -- No body, should return nothing
            result <- runEval (until_ args) (E.fromFrame lib)
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, _) -> res `shouldBe` Nothing

        it "executes body and modifies environment flag" do
            -- Set up environment with flag = false
            let initialEnv = E.defineVar "flag" (Symbol "false") ((E.fromFrame lib))
            -- until flag: set flag to true
            let args = [Symbol "flag", List [Symbol "set", Symbol "flag", Symbol "true"]]
            result <- runEval (until_ args) initialEnv
            case result of
                Left err -> expectationFailure $ "Until failed: " <> show err
                Right (res, finalEnv) -> do
                    res `shouldBe` Nothing
                    -- Check that flag was changed to true
                    E.lookupLocal "flag" finalEnv `shouldBe` Just (Symbol "true")

        it "fails with wrong number of arguments" do
            let args = [] -- No condition
            result <- runEval (until_ args) (E.fromFrame lib)
            result `shouldSatisfy` isLeft
