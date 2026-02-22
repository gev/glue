{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.SerializeSpec (spec) where

import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Glue.AST (AST (..))
import Glue.AST qualified as AST
import Glue.Parse (parseGlue)
import Glue.Serialize (serializeAST)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

validSymbolChar :: Gen Char
validSymbolChar = oneof [letterChar, digitChar, specialChar]

letterChar :: Gen Char
letterChar = choose ('a', 'z')

digitChar :: Gen Char
digitChar = choose ('0', '9')

specialChar :: Gen Char
specialChar = elements "+-*/%=<>&|!?$@#_.'"

genValidSymbol :: Gen T.Text
genValidSymbol = T.pack <$> ((:) <$> letterChar <*> listOf validSymbolChar)

genValidString :: Gen T.Text
genValidString = T.pack <$> listOf1 (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 +-.,/:;!?@#$%&*_")

instance Arbitrary AST where
    arbitrary = sized genGlue
      where
        genGlue n
            | n <= 0 =
                oneof
                    [ AST.Symbol <$> genValidSymbol
                    , AST.Integer <$> arbitrary
                    , AST.Float <$> arbitrary
                    , AST.String <$> genValidString
                    ]
        genGlue n =
            oneof
                [ AST.Symbol <$> genValidSymbol
                , AST.Integer <$> arbitrary
                , AST.Float <$> arbitrary
                , AST.String <$> genValidString
                , -- Generate nested structures
                  AST.List <$> resize (n `div` 2) arbitrary
                , AST.Object <$> resize (n `div` 2) (listOf1 $ (,) <$> genValidSymbol <*> arbitrary)
                ]

spec :: Spec
spec = describe "Glue Serialize" $ do
    describe "Roundtrip: AST -> Text -> AST" $ do
        it "serializes and parses back to same value" $
            property $
                \ast ->
                    let text = toStrict (serializeAST ast)
                        parsed = parseGlue text
                     in parsed == Right ast
