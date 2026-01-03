{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.TestUtils where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- | Arbitrary instance for IR values used in property testing
instance Arbitrary (IR Eval) where
    arbitrary = sized genIR
      where
        genIR n
            | n <= 0 = oneof [Number <$> arbitrary, String <$> arbitrary, Symbol <$> arbitrary]
            | otherwise =
                oneof
                    [ Number <$> arbitrary
                    , String <$> arbitrary
                    , Symbol <$> arbitrary
                    , List <$> resize (n `div` 2) arbitrary
                    ]
