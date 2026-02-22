{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.SerializeSpec (spec) where

import Data.Text.Lazy (toStrict)
import Glue.AST (AST (..))
import Glue.AST qualified as AST
import Glue.Parse (parseGlue)
import Glue.Serialize (serializeAST)
import Test.Hspec
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
spec = describe "Glue Serialize" $ do
    describe "Roundtrip: AST -> Text -> AST" $ do
        it "serializes and parses back to same value" $
            property $
                \ast ->
                    let text = toStrict (serializeAST ast)
                        parsed = parseGlue text
                     in parsed == Right ast
