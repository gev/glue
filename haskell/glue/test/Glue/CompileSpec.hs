{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.CompileSpec (spec) where

import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Glue.AST (AST)
import Glue.AST qualified as AST
import Glue.IR (IR, compile, getSymbol, isList, isObject, isSymbol, listLength, objectLookup, objectSize)
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
spec = describe "AST -> IR transformation (compile)" do
    describe "AST data constructors" do
        it "creates Integer literals" do
            AST.Integer 42 `shouldBe` AST.Integer 42

        it "creates Float literals" do
            AST.Float 3.14 `shouldBe` AST.Float 3.14

        it "creates String literals" do
            AST.String "hello" `shouldBe` AST.String "hello"

        it "creates Symbol literals" do
            AST.Symbol "x" `shouldBe` AST.Symbol "x"

        it "creates List expressions" do
            AST.List [AST.Integer 1, AST.Integer 2, AST.Integer 3] `shouldBe` AST.List [AST.Integer 1, AST.Integer 2, AST.Integer 3]

        it "creates Object expressions" do
            AST.Object [("key", AST.String "value")] `shouldBe` AST.Object [("key", AST.String "value")]

    describe "AST equality" do
        it "Integer equality" do
            AST.Integer 42 `shouldBe` AST.Integer 42
            AST.Integer 42 `shouldNotBe` AST.Integer 43

        it "Float equality" do
            AST.Float 3.14 `shouldBe` AST.Float 3.14
            AST.Float 3.14 `shouldNotBe` AST.Float 3.15

        it "String equality" do
            AST.String "hello" `shouldBe` AST.String "hello"
            AST.String "hello" `shouldNotBe` AST.String "world"

        it "Symbol equality" do
            AST.Symbol "x" `shouldBe` AST.Symbol "x"
            AST.Symbol "x" `shouldNotBe` AST.Symbol "y"

        it "List equality" do
            AST.List [AST.Integer 1, AST.Integer 2] `shouldBe` AST.List [AST.Integer 1, AST.Integer 2]
            AST.List [AST.Integer 1, AST.Integer 2] `shouldNotBe` AST.List [AST.Integer 1, AST.Integer 3]

        it "Object equality" do
            let obj1 = AST.Object [("a", AST.Integer 1)]
            let obj2 = AST.Object [("a", AST.Integer 1)]
            let obj3 = AST.Object [("a", AST.Integer 2)]
            obj1 `shouldBe` obj2
            obj1 `shouldNotBe` obj3

    describe "AST show instances" do
        it "shows Integer" do
            show (AST.Integer 42) `shouldBe` "42"

        it "shows Float" do
            show (AST.Float 3.14) `shouldBe` "3.14"

        it "shows String" do
            show (AST.String "hello") `shouldBe` "\"hello\""

        it "shows Symbol" do
            show (AST.Symbol "x") `shouldBe` "x"

        it "shows List" do
            show (AST.List [AST.Integer 1, AST.Integer 2]) `shouldBe` "(1 2)"

        it "shows Object" do
            show (AST.Object [("key", AST.String "value")]) `shouldBe` "(:key \"value\")"

    describe "AST type distinctions" do
        it "distinguishes Integer from Float" do
            AST.Integer 42 `shouldNotBe` AST.Float 42.0

        it "distinguishes different AST types" do
            AST.Integer 42 `shouldNotBe` AST.String "42"
            AST.Symbol "42" `shouldNotBe` AST.String "42"

    describe "Complex AST structures" do
        it "handles nested Lists" do
            let nested = AST.List [AST.List [AST.Integer 1, AST.Integer 2], AST.Integer 3]
            show nested `shouldBe` "((1 2) 3)"

        it "handles nested Objects" do
            let inner = AST.Object [("b", AST.Integer 2)]
            let outer = AST.Object [("a", inner)]
            show outer `shouldBe` "(:a (:b 2))"

        it "handles mixed types in Lists" do
            let mixed = AST.List [AST.Integer 1, AST.Float 2.5, AST.String "hello", AST.Symbol "x"]
            show mixed `shouldBe` "(1 2.5 \"hello\" x)"

        it "handles empty structures" do
            show (AST.List []) `shouldBe` "()"
            show (AST.Object []) `shouldBe` "(:)"

    describe "AST -> IR compilation" do
        -- Positional lists
        prop "atoms: list length is preserved 1 to 1" $ \(xs :: [AST]) -> do
            let ast = AST.List xs
            let val = compile ast :: IR Identity
            if isList val
                then listLength val `shouldBe` length xs
                else expectationFailure "Expected List"

        -- Property lists (key-value)
        prop "properties: Object size <= number of properties (duplicates removed)" $ \(ps :: [(T.Text, AST)]) -> do
            let ast = AST.Object ps
            let val = compile ast :: IR Identity
            if isObject val
                then objectSize val `shouldSatisfy` (<= length ps)
                else expectationFailure "Expected Object"

        -- Recursive integrity (doesn't crash on deep trees)
        prop "integrity: any AST structure transforms successfully" $ \(ast :: AST) ->
            let val = compile ast :: IR Identity
             in -- If compile doesn't throw exception, test passes
                seq val True `shouldBe` True

        prop "empty atoms and props don't create garbage data" do
            let ast1 = AST.List []
            let ast2 = AST.Object []
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
            let ast = AST.Object [(k, astInner)]
            let val = compile ast :: IR Identity
            if isObject val
                then case objectLookup k val of
                    Just valInner -> valInner `shouldBe` (compile astInner :: IR Identity)
                    Nothing -> expectationFailure "Key not found"
                else expectationFailure "Recursive expansion error"
