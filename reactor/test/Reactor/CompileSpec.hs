{-# OPTIONS_GHC -Wno-orphans #-}

module Reactor.CompileSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Functor.Identity (Identity)
import Reactor.AST (AST)
import Reactor.AST qualified as AST
import Reactor.IR (IR, compile)
import Reactor.IR qualified as IR

-- 1. Generator for Reactor AST itself
instance Arbitrary AST where
    arbitrary = sized genReactor
      where
        genReactor n
            | n <= 0 =
                oneof
                    [ AST.Symbol <$> arbitrary
                    , AST.Number <$> arbitrary
                    , AST.String <$> arbitrary
                    ]
        genReactor n =
            oneof
                [ AST.Symbol <$> arbitrary
                , AST.Number <$> arbitrary
                , AST.String <$> arbitrary
                , -- Generate nested structures
                  AST.AtomList <$> resize (n `div` 2) arbitrary
                , AST.PropList <$> resize (n `div` 2) arbitrary
                , AST.PropAccess <$> resize (n `div` 2) arbitrary <*> arbitrary
                ]

spec :: Spec
spec = describe "AST -> IR transformation (compile)" do
    -- Positional lists
    prop "atoms: list length is preserved 1 to 1" $ \(xs :: [AST]) -> do
        let ast = AST.AtomList xs
        let val = compile ast :: IR Identity
        case val of
            IR.AtomList ys -> length ys `shouldBe` length xs
            _ -> expectationFailure "Expected AtomList"

    -- Property lists (key-value)
    prop "properties: PropList length equals number of properties" $ \(ps :: [(T.Text, AST)]) -> do
        let ast = AST.PropList ps
        let val = compile ast :: IR Identity
        case val of
            IR.PropList ys -> length ys `shouldBe` length ps
            _ -> expectationFailure "Expected PropList"

    -- Recursive integrity (doesn't crash on deep trees)
    prop "integrity: any AST structure transforms successfully" $ \(ast :: AST) -> do
        let val = compile ast :: IR Identity
        -- If compile doesn't throw exception, test passes
        seq val True `shouldBe` True

    prop "empty atoms and props don't create garbage data" do
        let ast1 = AST.AtomList []
        let ast2 = AST.PropList []
        (compile ast1 :: IR Identity) `shouldBe` IR.AtomList []
        (compile ast2 :: IR Identity) `shouldBe` IR.PropList []

    prop "Symbol becomes Symbol unchanged (idempotent)" $ \s -> do
        let ast = AST.Symbol s
        let val = compile ast :: IR Identity
        case val of
            IR.Symbol s' -> s' `shouldBe` s
            _ -> expectationFailure "Should be Symbol"

    prop "recursive expansion: properties can contain nested calls" $ \k (astInner :: AST) -> do
        let ast = AST.PropList [(k, astInner)]
        let val = compile ast :: IR Identity
        case val of
            IR.PropList [(k', valInner)] -> do
                k' `shouldBe` k
                valInner `shouldBe` (compile astInner :: IR Identity)
            _ -> expectationFailure "Recursive expansion error"

    prop "PropAccess compiles correctly" $ \(obj :: AST) prop -> do
        let ast = AST.PropAccess obj prop
        let val = compile ast :: IR Identity
        val `shouldBe` IR.PropAccess (compile obj) prop
