{-# OPTIONS_GHC -Wno-orphans #-}

module Reactor.CompileSpec (spec) where

import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Reactor.AST (AST)
import Reactor.AST qualified as AST
import Reactor.IR (IR, compile, getPropAccess, getSymbol, isList, isObject, isPropAccess, isSymbol, listLength, objectLookup, objectSize)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

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
        if isList val
            then listLength val `shouldBe` length xs
            else expectationFailure "Expected List"

    -- Property lists (key-value)
    prop "properties: Object size <= number of properties (duplicates removed)" $ \(ps :: [(T.Text, AST)]) -> do
        let ast = AST.PropList ps
        let val = compile ast :: IR Identity
        if isObject val
            then objectSize val `shouldSatisfy` (<= length ps)
            else expectationFailure "Expected Object"

    -- Recursive integrity (doesn't crash on deep trees)
    prop "integrity: any AST structure transforms successfully" $ \(ast :: AST) -> do
        let val = compile ast :: IR Identity
        -- If compile doesn't throw exception, test passes
        seq val True `shouldBe` True

    prop "empty atoms and props don't create garbage data" do
        let ast1 = AST.AtomList []
        let ast2 = AST.PropList []
        let val1 = compile ast1 :: IR Identity
        let val2 = compile ast2 :: IR Identity
        (isList val1 && listLength val1 == 0) `shouldBe` True
        (isObject val2 && objectSize val2 == 0) `shouldBe` True

    prop "Symbol becomes Symbol unchanged (idempotent)" $ \s -> do
        let ast = AST.Symbol s
        let val = compile ast :: IR Identity
        if isSymbol val
            then getSymbol val `shouldBe` s
            else expectationFailure "Should be Symbol"

    prop "recursive expansion: properties can contain nested calls" $ \k (astInner :: AST) -> do
        let ast = AST.PropList [(k, astInner)]
        let val = compile ast :: IR Identity
        if isObject val
            then case objectLookup k val of
                Just valInner -> valInner `shouldBe` (compile astInner :: IR Identity)
                Nothing -> expectationFailure "Key not found"
            else expectationFailure "Recursive expansion error"

    prop "PropAccess compiles correctly" $ \(obj :: AST) pro -> do
        let ast = AST.PropAccess obj pro
        let val = compile ast :: IR Identity
        if isPropAccess val
            then do
                let (o, p) = getPropAccess val
                o `shouldBe` compile obj
                p `shouldBe` pro
            else expectationFailure "Expected PropAccess"
