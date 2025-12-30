module Reactor.Lib.Builtin.QuoteSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Instances ()

import Data.Either (isLeft)
import Reactor.Env qualified as E
import Reactor.Eval (runEvalLegacy)
import Reactor.IR
import Reactor.Lib.Builtin.Quote (makeQuote, quote)

spec :: Spec
spec = describe "Reactor.Lib.Builtin.Quote (Test quotes)" do
    describe "makeQuote: pure validation" do
        it "successfully return exactly one argument" do
            makeQuote [Number 42] `shouldBe` Right (Number 42)

        it "returns error if no arguments or too many" do
            makeQuote [] `shouldSatisfy` isLeft
            makeQuote [Number 1, Number 2] `shouldSatisfy` isLeft

    describe "quote: special form" do
        it "quotes a value" do
            let initialEnv = E.emptyEnv
            let args = [Number 42]
            result <- runEvalLegacy (quote args) initialEnv
            case result of
                Left err -> expectationFailure $ "Quote failed: " <> show err
                Right (res, _, _) -> res `shouldBe` Just (Number 42)

        it "fails with wrong number of arguments" do
            let initialEnv = E.emptyEnv
            let args = []
            result <- runEvalLegacy (quote args) initialEnv
            result `shouldSatisfy` isLeft
