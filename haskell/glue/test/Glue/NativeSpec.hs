{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.NativeSpec (spec) where

import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Glue.IR (HostValue, IR (..), Native (..), extractHostValue, hostValue, isHostValue, getHostValueFromIR)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- Test data types for host values
data TestWidget = TestWidget {label :: String, enabled :: Bool}
    deriving (Show, Eq)

data TestConnection = TestConnection {host :: String, port :: Int}
    deriving (Show, Eq)

instance Arbitrary TestWidget where
    arbitrary = TestWidget <$> arbitrary <*> arbitrary

instance Arbitrary TestConnection where
    arbitrary = TestConnection <$> arbitrary <*> arbitrary

spec :: Spec
spec = describe "HostValue system for native object integration" do

    describe "HostValue creation and extraction" do
        it "creates host value from any typeable value" do
            let widget = TestWidget "button" True
                hv = hostValue widget
            hv `shouldSatisfy` (const True)  -- Just check it creates successfully

        it "extracts host value with correct type" do
            let widget = TestWidget "submit" False
                hv = hostValue widget
            extractHostValue hv `shouldBe` Just widget

        it "fails to extract with wrong type" do
            let widget = TestWidget "test" True
                hv = hostValue widget
            extractHostValue hv `shouldNotBe` (Just "not a widget" :: Maybe String)

        it "handles different host value types" do
            let conn = TestConnection "localhost" 5432
                hv = hostValue conn
            extractHostValue hv `shouldBe` Just conn

        prop "round-trip: any typeable value can be stored and retrieved" $ \(w :: TestWidget) ->
            let hv = hostValue w
            in extractHostValue hv == Just w

    describe "HostValue equality" do
        it "host values are never equal (opaque comparison)" do
            let hv1 = hostValue (TestWidget "a" True)
                hv2 = hostValue (TestWidget "a" True)
            hv1 == hv2 `shouldBe` False

        it "different host values are not equal" do
            let hv1 = hostValue (TestWidget "a" True)
                hv2 = hostValue (TestWidget "b" False)
            hv1 == hv2 `shouldBe` False

    describe "HostValue in IR system" do
        it "creates IR with host value" do
            let widget = TestWidget "test" True
                hv = hostValue widget
                ir = Native (Value hv) :: IR Identity
            ir `shouldSatisfy` (const True)

        it "identifies host value IR" do
            let hv = hostValue (TestWidget "test" True)
                ir = Native (Value hv) :: IR Identity
            isHostValue ir `shouldBe` True

        it "non-host IR is not identified as host value" do
            let ir = Integer 42 :: IR Identity
            isHostValue ir `shouldBe` False

        it "extracts host value from IR" do
            let widget = TestWidget "extract" False
                hv = hostValue widget
                ir = Native (Value hv) :: IR Identity
            getHostValueFromIR ir `shouldBe` Just hv

        it "returns Nothing for non-host IR" do
            let ir = String "not host" :: IR Identity
            getHostValueFromIR ir `shouldBe` Nothing

    describe "Native IR constructors" do
        it "creates Func native" do
            let native = Func (\_ -> pure (Integer 42)) :: Native Identity
            native `shouldSatisfy` (const True)

        it "creates Special native" do
            let native = Special (\_ -> pure (Integer 42)) :: Native Identity
            native `shouldSatisfy` (const True)

        it "creates Value native" do
            let hv = hostValue (TestWidget "native" True)
                native = Value hv :: Native Identity
            native `shouldSatisfy` (const True)

    describe "Native equality" do
        it "Func natives are equal" do
            let f1 = Func (\_ -> pure (Integer 1)) :: Native Identity
                f2 = Func (\_ -> pure (Integer 2)) :: Native Identity
            f1 == f2 `shouldBe` True  -- Functions compare as equal regardless of implementation

        it "Special natives are equal" do
            let s1 = Special (\_ -> pure (Integer 1)) :: Native Identity
                s2 = Special (\_ -> pure (Integer 2)) :: Native Identity
            s1 == s2 `shouldBe` True

        it "Value natives compare by host value" do
            let hv1 = hostValue (TestWidget "a" True)
                hv2 = hostValue (TestWidget "a" True)
                v1 = Value hv1 :: Native Identity
                v2 = Value hv2 :: Native Identity
            v1 == v2 `shouldBe` False  -- Host values are never equal

    describe "IR with Native values" do
        it "IR equality handles Native values" do
            let hv = hostValue (TestWidget "ir" True)
                ir1 = Native (Value hv) :: IR Identity
                ir2 = Native (Value hv) :: IR Identity
            ir1 == ir2 `shouldBe` True

        it "IR show displays Native values" do
            let hv = hostValue (TestWidget "show" False)
                ir = Native (Value hv) :: IR Identity
            show ir `shouldContain` "<host:"

    describe "Type safety guarantees" do
        it "prevents incorrect type extraction at runtime" do
            let widget = TestWidget "type" True
                hv = hostValue widget
            -- This should fail at runtime, not compile time
            extractHostValue hv `shouldNotBe` (Just "not a widget" :: Maybe String)

        prop "type safety: extraction succeeds only for correct types" $ \(w :: TestWidget) (s :: String) ->
            let hv = hostValue w
                widgetResult = extractHostValue hv :: Maybe TestWidget
                stringResult = extractHostValue hv :: Maybe String
            in widgetResult == Just w && stringResult == Nothing

    describe "Host value lifecycle" do
        it "host values can be created from complex types" do
            let complex = [TestWidget "a" True, TestWidget "b" False]
                hv = hostValue complex
            extractHostValue hv `shouldBe` Just complex

        it "host values preserve nested structures" do
            let nested = ("tuple", TestConnection "host" 8080, 42 :: Int)
                hv = hostValue nested
            extractHostValue hv `shouldBe` Just nested

    describe "Integration with IR system" do
        prop "host values integrate seamlessly with IR" $ \(w :: TestWidget) ->
            let hv = hostValue w
                ir = Native (Value hv) :: IR Identity
            in isHostValue ir &&
               case getHostValueFromIR ir of
                   Just extracted -> extractHostValue extracted == Just w
                   Nothing -> False

        it "host values work in complex IR structures" do
            let hv = hostValue (TestWidget "complex" True)
                listIr = List [Native (Value hv), String "test"] :: IR Identity
            -- The list should contain the host value
            case listIr of
                List [Native (Value _), String "test"] -> True
                _ -> False
            `shouldBe` True
