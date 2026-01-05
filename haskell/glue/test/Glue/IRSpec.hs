{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.IRSpec (spec) where

import Data.Functor.Identity (Identity)
import Glue.IR (IR (..))
import Test.Hspec

spec :: Spec
spec = describe "IR data types" do
    describe "Exception IR values" do
        it "creates Exception with type and payload" do
            let exc = Exception "DivByZero" (String "cannot divide by zero") :: IR Identity
            exc `shouldSatisfy` isException

        it "Exception displays correctly" do
            let exc = Exception "TestError" (Number 42) :: IR Identity
            show exc `shouldBe` "{exception TestError 42.0}"

        it "Exception equality works" do
            let exc1 = Exception "Error" (String "msg") :: IR Identity
            let exc2 = Exception "Error" (String "msg") :: IR Identity
            let exc3 = Exception "Error" (String "different") :: IR Identity
            exc1 `shouldBe` exc2
            exc1 `shouldNotBe` exc3

        it "Exception with different types are not equal" do
            let exc1 = Exception "TypeA" (Number 1) :: IR Identity
            let exc2 = Exception "TypeB" (Number 1) :: IR Identity
            exc1 `shouldNotBe` exc2

        it "Exception with different payloads are not equal" do
            let exc1 = Exception "SameType" (Number 1) :: IR Identity
            let exc2 = Exception "SameType" (Number 2) :: IR Identity
            exc1 `shouldNotBe` exc2

        it "Exception preserves error type and payload" do
            let exc = Exception "CustomError" (Number 123) :: IR Identity
            case exc of
                Exception t p -> do
                    t `shouldBe` "CustomError"
                    p `shouldBe` Number 123
                _ -> expectationFailure "Should be Exception"

        it "Exception structure integrity" do
            let exc = Exception "TestErr" (String "payload") :: IR Identity
            show exc `shouldContain` "exception"
            show exc `shouldContain` "TestErr"

-- Helper function
isException :: IR m -> Bool
isException (Exception _ _) = True
isException _ = False
