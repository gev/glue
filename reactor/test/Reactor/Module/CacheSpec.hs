module Reactor.Module.CacheSpec where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (IR (..))
import Reactor.Module (ImportedModule (..))
import Reactor.Module.Cache qualified as Cache
import Test.Hspec

spec :: Spec
spec = do
    describe "ImportedModuleCache operations" $ do
        it "emptyCache creates empty cache" $ do
            let cache = Cache.emptyCache :: Cache.ImportedModuleCache Eval
            Cache.lookupCachedModule "nonexistent" cache `shouldBe` Nothing

        it "caches and retrieves imported module" $ do
            let emptyEnv = E.emptyEnv
            let exportedValues = E.frameFromList [("value", Number 42)]
            let importedModule = ImportedModule
                    { moduleName = "test.module"
                    , exportedValues = exportedValues
                    , evaluationRootEnv = emptyEnv
                    }

            -- Start with empty cache
            let cache = Cache.emptyCache :: Cache.ImportedModuleCache Eval

            -- Cache the module
            let cached = Cache.cacheModule importedModule cache

            -- Retrieve the cached module
            Cache.lookupCachedModule "test.module" cached `shouldBe` Just importedModule

        it "handles missing modules in cache" $ do
            let emptyEnv = E.emptyEnv
            let exportedValues = E.frameFromList [("func", Number 123)]
            let importedModule = ImportedModule
                    { moduleName = "existing.module"
                    , exportedValues = exportedValues
                    , evaluationRootEnv = emptyEnv
                    }

            -- Cache one module
            let cache = Cache.cacheModule importedModule Cache.emptyCache

            -- Try to lookup a different module
            Cache.lookupCachedModule "nonexistent.module" cache `shouldBe` Nothing

        it "overwrites existing cached module" $ do
            let emptyEnv = E.emptyEnv

            -- First module
            let module1 = ImportedModule
                    { moduleName = "test.module"
                    , exportedValues = E.frameFromList [("value", Number 1)]
                    , evaluationRootEnv = emptyEnv
                    }

            -- Second module with same name but different content
            let module2 = ImportedModule
                    { moduleName = "test.module"
                    , exportedValues = E.frameFromList [("value", Number 2)]
                    , evaluationRootEnv = emptyEnv
                    }

            -- Cache first module
            let cache1 = Cache.cacheModule module1 Cache.emptyCache

            -- Cache second module (should overwrite)
            let cache2 = Cache.cacheModule module2 cache1

            -- Should return the second module
            Cache.lookupCachedModule "test.module" cache2 `shouldBe` Just module2

        it "caches multiple different modules" $ do
            let emptyEnv = E.emptyEnv

            let module1 = ImportedModule
                    { moduleName = "math.add"
                    , exportedValues = E.frameFromList [("add", Number 1)]
                    , evaluationRootEnv = emptyEnv
                    }

            let module2 = ImportedModule
                    { moduleName = "math.mul"
                    , exportedValues = E.frameFromList [("mul", Number 2)]
                    , evaluationRootEnv = emptyEnv
                    }

            -- Cache both modules
            let cache = Cache.cacheModule module1 Cache.emptyCache
            let finalCache = Cache.cacheModule module2 cache

            -- Both should be retrievable
            Cache.lookupCachedModule "math.add" finalCache `shouldBe` Just module1
            Cache.lookupCachedModule "math.mul" finalCache `shouldBe` Just module2
