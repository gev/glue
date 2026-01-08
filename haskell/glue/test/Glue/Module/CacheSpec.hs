module Glue.Module.CacheSpec where

import Glue.Env qualified as E
import Glue.IR (IR (..))
import Glue.Module (ImportedModule (..))
import Glue.Module.Cache (cacheModule, emptyCache, lookupCachedModule)
import Test.Hspec

spec :: Spec
spec = do
    describe "ImportedModuleCache operations" $ do
        it "emptyCache creates empty cache" $ do
            let cache = emptyCache
            lookupCachedModule "nonexistent" cache `shouldBe` Nothing

        it "caches and retrieves imported module" $ do
            let emptyEnv = E.emptyEnv
            let exportedValues = E.frameFromList [("value", Integer 42)]
            let importedModule =
                    ImportedModule
                        { moduleName = "test.module"
                        , exportedValues = exportedValues
                        , evaluationRootEnv = emptyEnv
                        }

            -- Start with empty cache
            let cache = emptyCache

            -- Cache the module
            let cached = cacheModule importedModule cache

            -- Retrieve the cached module
            lookupCachedModule "test.module" cached `shouldBe` Just importedModule

        it "handles missing modules in cache" $ do
            let emptyEnv = E.emptyEnv
            let exportedValues = E.frameFromList [("func", Integer 123)]
            let importedModule =
                    ImportedModule
                        { moduleName = "existing.module"
                        , exportedValues = exportedValues
                        , evaluationRootEnv = emptyEnv
                        }

            -- Cache one module
            let cache = cacheModule importedModule emptyCache

            -- Try to lookup a different module
            lookupCachedModule "nonexistent.module" cache `shouldBe` Nothing

        it "overwrites existing cached module" $ do
            let emptyEnv = E.emptyEnv

            -- First module
            let module1 =
                    ImportedModule
                        { moduleName = "test.module"
                        , exportedValues = E.frameFromList [("value", Integer 1)]
                        , evaluationRootEnv = emptyEnv
                        }

            -- Second module with same name but different content
            let module2 =
                    ImportedModule
                        { moduleName = "test.module"
                        , exportedValues = E.frameFromList [("value", Integer 2)]
                        , evaluationRootEnv = emptyEnv
                        }

            -- Cache first module
            let cache1 = cacheModule module1 emptyCache

            -- Cache second module (should overwrite)
            let cache2 = cacheModule module2 cache1

            -- Should return the second module
            lookupCachedModule "test.module" cache2 `shouldBe` Just module2

        it "caches multiple different modules" $ do
            let emptyEnv = E.emptyEnv

            let module1 =
                    ImportedModule
                        { moduleName = "math.add"
                        , exportedValues = E.frameFromList [("add", Integer 1)]
                        , evaluationRootEnv = emptyEnv
                        }

            let module2 =
                    ImportedModule
                        { moduleName = "math.mul"
                        , exportedValues = E.frameFromList [("mul", Integer 2)]
                        , evaluationRootEnv = emptyEnv
                        }

            -- Cache both modules
            let cache = cacheModule module1 emptyCache
            let finalCache = cacheModule module2 cache

            -- Both should be retrievable
            lookupCachedModule "math.add" finalCache `shouldBe` Just module1
            lookupCachedModule "math.mul" finalCache `shouldBe` Just module2
