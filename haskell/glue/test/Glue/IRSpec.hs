{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.IRSpec (spec) where

import Data.Functor.Identity (Identity)
import Glue.IR (IR (..))
import Test.Hspec

spec :: Spec
spec = describe "IR data types" do
    it "has various constructors" do
        let num = Number 42 :: IR Identity
        let str = String "hello" :: IR Identity
        let sym = Symbol "x" :: IR Identity
        num `shouldSatisfy` (\case Number _ -> True; _ -> False)
        str `shouldSatisfy` (\case String _ -> True; _ -> False)
        sym `shouldSatisfy` (\case Symbol _ -> True; _ -> False)
