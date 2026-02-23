{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.DecompileSpec (spec) where

import Data.Functor.Identity (Identity)
import Glue.AST (AST)
import Glue.AST qualified as AST
import Glue.Compile (compile)
import Glue.Decompile (decompile)
import Glue.IR (IR)
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

spec :: Spec
spec = describe "IR -> AST transformation (decompile)" $ do
    describe "Roundtrip: IR -> AST -> IR" $ do
        prop "decompile then compile returns equivalent IR" $ \(ast :: AST) -> do
            let ir1 = compile ast :: IR Identity
            case decompile ir1 of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> do
                    let ir2 = compile ast2 :: IR Identity
                    show ir1 `shouldBe` show ir2
