module Reactor.Module.RegistrySpec where

import Reactor.Eval (Eval)
import Reactor.IR (IR (..))
import Reactor.Module.Registry qualified as Registry
import Test.Hspec

spec :: Spec
spec = do
    describe "ModuleRegistry operations" $ do
        it "emptyRegistry creates empty registry" $ do
            let registry = Registry.emptyRegistry :: Registry.ModuleRegistry (IR Eval)
            Registry.lookupModule "nonexistent" registry `shouldBe` Nothing

        it "registrySize returns correct count" $ do
            let emptyReg = Registry.emptyRegistry :: Registry.ModuleRegistry (IR Eval)
            Registry.registrySize emptyReg `shouldBe` 0
