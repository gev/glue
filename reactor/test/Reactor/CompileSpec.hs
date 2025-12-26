{-# OPTIONS_GHC -Wno-orphans #-}

module Reactor.CompileSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as Map
import Data.Monoid (mempty)
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
            IR.List ys -> length ys `shouldBe` length xs
            _ -> expectationFailure "Expected List"

    -- Property lists (key-value)
    prop "properties: Object size <= number of properties (duplicates removed)" $ \(ps :: [(T.Text, AST)]) -> do
        let ast = AST.PropList ps
        let val = compile ast :: IR Identity
        case val of
            IR.Object objMap -> Map.size objMap `shouldSatisfy` (<= length ps)
            _ -> expectationFailure "Expected Object"

    -- Recursive integrity (doesn't crash on deep trees)
    prop "integrity: any AST structure transforms successfully" $ \(ast :: AST) -> do
        let val = compile ast :: IR Identity
        -- If compile doesn't throw exception, test passes
        seq val True `shouldBe` True

    prop "empty atoms and props don't create garbage data" do
        let ast1 = AST.AtomList []
        let ast2 = AST.PropList []
        (compile ast1 :: IR Identity) `shouldBe` IR.List []
        (compile ast2 :: IR Identity) `shouldBe` IR.Object mempty

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
            IR.Object objMap -> case Map.lookup k objMap of
                Just valInner -> valInner `shouldBe` (compile astInner :: IR Identity)
                Nothing -> expectationFailure "Key not found"
            _ -> expectationFailure "Recursive expansion error"

    prop "PropAccess compiles correctly" $ \(obj :: AST) pro -> do
        let ast = AST.PropAccess obj pro
        let val = compile ast :: IR Identity
        val `shouldBe` IR.PropAccess (compile obj) pro
