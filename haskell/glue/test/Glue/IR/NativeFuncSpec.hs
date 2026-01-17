module Glue.IR.NativeFuncSpec (spec) where

import Glue.IR (IR (..))
import Test.Hspec

spec :: Spec
spec = describe "NativeFunc constructors and equality" do
    it "creates NativeFunc" do
        let nf = NativeFunc (\_ -> pure (Integer 42)) :: IR Identity
        nf `shouldSatisfy` const True

    it "NativeFunc are equal" do
        let f1 = NativeFunc (\_ -> pure (Integer 1)) :: IR Identity
            f2 = NativeFunc (\_ -> pure (Integer 2)) :: IR Identity
        f1 == f2 `shouldBe` True -- Functions compare as equal regardless of implementation
