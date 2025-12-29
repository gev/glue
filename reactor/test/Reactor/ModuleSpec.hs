module Reactor.ModuleSpec where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Eval (Eval)
import Reactor.IR (IR (..))
import Reactor.Module (Module (..), ModuleRegistry)
import Test.Hspec

spec :: Spec
spec = do
    describe "Module data structure" $ do
        it "creates module correctly" $ do
            let mod = Module
                    { name = "test.module"
                    , exports = ["func1", "func2"]
                    , body = [Symbol "def", Symbol "func1", Number 42]
                    }
            name mod `shouldBe` "test.module"
            exports mod `shouldBe` ["func1", "func2"]
            length (body mod) `shouldBe` 3

    describe "ModuleRegistry operations" $ do
        it "empty registry" $ do
            let registry = Map.empty :: ModuleRegistry Eval
            Map.size registry `shouldBe` 0

        it "adds and retrieves modules" $ do
            let mod = Module "test.mod" ["x"] [Number 1]
                registry = Map.singleton "test.mod" mod
            case Map.lookup "test.mod" registry of
                Just m -> name m `shouldBe` "test.mod"
                Nothing -> expectationFailure "Module not found"

        it "handles missing modules" $ do
            let registry = Map.empty :: ModuleRegistry Eval
            Map.lookup "nonexistent" registry `shouldBe` Nothing
