module Reactor.Module.RegistrySpec where

import Reactor.Module.Registry (emptyRegistry, lookupModule, registrySize)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "ModuleRegistry operations" $ do
        it "emptyRegistry creates empty registry" $ do
            let registry = emptyRegistry
            lookupModule "nonexistent" registry `shouldBe` Nothing

        it "registrySize returns correct count" $ do
            let emptyReg = emptyRegistry
            registrySize emptyReg `shouldBe` 0
