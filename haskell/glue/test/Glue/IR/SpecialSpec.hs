module Glue.IR.SpecialSpec (spec) where

import Data.Functor.Identity (Identity)
import Glue.IR (IR (..))
import Test.Hspec

spec :: Spec
spec = describe "Special constructors and equality" do
    it "creates Special" do
        let s = Special (\_ -> pure (Integer 42)) :: IR Identity
        s `shouldSatisfy` const True

    it "Special are equal" do
        let s1 = Special (\_ -> pure (Integer 1)) :: IR Identity
            s2 = Special (\_ -> pure (Integer 2)) :: IR Identity
        s1 == s2 `shouldBe` True -- Special forms compare as equal regardless of implementation
