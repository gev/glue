module Reactor.Lib.Builtin.QuoteSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Instances ()

import Data.Either (isLeft)
import Reactor.IR
import Reactor.Lib.Builtin.Quote (makeQuote)

spec :: Spec
spec = describe "Reactor.Lib.Builtin.Quote (Test quotes)" do
    describe "Special form and validations" do
        it "makeQuote: successfully return exactly one argument" do
            makeQuote [Number 42] `shouldBe` Right (Number 42)

        it "makeQuote: returns error if no arguments or too many" do
            makeQuote [] `shouldSatisfy` isLeft
            makeQuote [Number 1, Number 2] `shouldSatisfy` isLeft
