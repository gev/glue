{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.DecompileSpec (spec) where

import Data.Functor.Identity (Identity)
import Glue.AST (AST)
import Glue.AST qualified as AST
import Glue.Compile (compile)
import Glue.Decompile (decompile)
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
    describe "Roundtrip: AST -> IR -> AST" $ do
        prop "compile then decompile returns original AST" $ \(ast :: AST) -> do
            let ir = compile ast :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast `shouldBe` ast2

        prop "atoms: Integer roundtrip" $ \(n :: Int) -> do
            let ast1 = AST.Integer n
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "atoms: Float roundtrip" $ \(n :: Double) -> do
            let ast1 = AST.Float n
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "atoms: String roundtrip" $ \s -> do
            let ast1 = AST.String s
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "atoms: Symbol roundtrip" $ \s -> do
            let ast1 = AST.Symbol s
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "List roundtrip" $ \(xs :: [AST]) -> do
            let ast1 = AST.List xs
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "Object roundtrip" $ \(ps :: [(String, AST)]) -> do
            let ast1 = AST.Object ps
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "empty List roundtrip" $ do
            let ast1 = AST.List []
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "empty Object roundtrip" $ do
            let ast1 = AST.Object []
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2

        prop "nested structures roundtrip" $ \(ast :: AST) -> do
            let ast1 = AST.List [AST.Object [("nested", ast)]]
            let ir = compile ast1 :: IR Identity
            case decompile ir of
                Left _ -> expectationFailure "Expected Right"
                Right ast2 -> ast1 `shouldBe` ast2
