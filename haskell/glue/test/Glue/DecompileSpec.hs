{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.DecompileSpec (spec) where

import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as Map
import Glue.AST (AST)
import Glue.AST qualified as AST
import Glue.Compile (compile)
import Glue.Decompile (decompile)
import Glue.IR (IR (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary AST where
    arbitrary = sized genGlue
      where
        genGlue n
            | n <= 0 =
                oneof
                    [ AST.Symbol <$> arbitrary
                    , AST.Integer <$> arbitrary
                    , AST.Float <$> arbitrary
                    , AST.String <$> arbitrary
                    ]
        genGlue n =
            oneof
                [ AST.Symbol <$> arbitrary
                , AST.Integer <$> arbitrary
                , AST.Float <$> arbitrary
                , AST.String <$> arbitrary
                , -- Generate nested structures
                  AST.List <$> resize (n `div` 2) arbitrary
                , AST.Object <$> resize (n `div` 2) arbitrary
                ]

-- Arbitrary for IR (only serializable types)
instance Arbitrary (IR m) where
    arbitrary = sized genIR
      where
        genIR n
            | n <= 0 =
                oneof
                    [ Integer <$> arbitrary
                    , Float <$> arbitrary
                    , String <$> arbitrary
                    , Symbol <$> arbitrary
                    ]
        genIR n =
            oneof
                [ Integer <$> arbitrary
                , Float <$> arbitrary
                , String <$> arbitrary
                , Symbol <$> arbitrary
                , -- Generate nested structures
                  List <$> resize (n `div` 2) (listOf1 (genIR (n `div` 2)))
                , Object <$> resize (n `div` 2) (Map.fromList <$> listOf1 ((,) <$> arbitrary <*> resize (n `div` 2) arbitrary))
                ]

spec :: Spec
spec = describe "IR -> AST transformation (decompile)" $ do
    describe "Roundtrip: IR -> AST -> IR" $ do
        prop "decompile then compile returns equivalent IR" $ \(ir :: IR Identity) -> do
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast -> do
                    let ir2 = compile ast :: IR Identity
                    show ir `shouldBe` show ir2
