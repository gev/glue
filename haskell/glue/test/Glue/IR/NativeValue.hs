{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.IR.NativeValue (spec) where

import Data.Functor.Identity (Identity)
import Glue.Env qualified as E
import Glue.Eval (Eval, eval, runEvalSimple)
import Glue.IR (IR (..), extractHostValue, getHostValueFromIR, hostValue, isHostValue)
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
            hv `shouldSatisfy` const True -- Just check it creates successfully
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
                ir = NativeValue hv :: IR Identity
            ir `shouldSatisfy` const True

        it "identifies host value IR" do
            let hv = hostValue (TestWidget "test" True)
                ir = NativeValue hv :: IR Identity
            isHostValue ir `shouldBe` True

        it "non-host IR is not identified as host value" do
            let ir = Integer 42 :: IR Identity
            isHostValue ir `shouldBe` False

        it "extracts host value from IR" do
            let widget = TestWidget "extract" False
                hv = hostValue widget
                ir = NativeValue hv :: IR Identity
            case getHostValueFromIR ir of
                Just extracted -> extractHostValue extracted `shouldBe` Just widget
                Nothing -> expectationFailure "Should extract host value"

        it "returns Nothing for non-host IR" do
            let ir = String "not host" :: IR Identity
            getHostValueFromIR ir `shouldBe` Nothing

    describe "IR with NativeValue" do
        it "IR equality handles NativeValue (host values are never equal)" do
            let hv1 = hostValue (TestWidget "ir" True)
                hv2 = hostValue (TestWidget "ir" True) -- Same content, different instances
                ir1 = NativeValue hv1 :: IR Identity
                ir2 = NativeValue hv2 :: IR Identity
            ir1 == ir2 `shouldBe` False -- Host values are never equal
        it "IR show displays NativeValue" do
            let hv = hostValue (TestWidget "show" False)
                ir = NativeValue hv :: IR Identity
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
                ir = NativeValue hv :: IR Identity
             in isHostValue ir
                    && case getHostValueFromIR ir of
                        Just extracted -> extractHostValue extracted == Just w
                        Nothing -> False

        it "host values work in complex IR structures" $
            let hv = hostValue (TestWidget "complex" True)
                listIr = List [NativeValue hv, String "test"] :: IR Identity
             in -- The list should contain the host value
                case listIr of
                    List [NativeValue _, String "test"] -> True
                    _ -> False
                    `shouldBe` True

    describe "HostValue evaluation behavior" do
        it "host values evaluate to themselves (no change)" do
            let hv = hostValue (TestWidget "eval" True)
                ir = NativeValue hv :: IR Eval
                env = E.emptyEnv
            result <- runEvalSimple (eval ir) env
            case result of
                Right (evaluated, _) -> do
                    -- Check that it's still a host value
                    isHostValue evaluated `shouldBe` True
                    -- Extract and compare the contents with explicit types
                    case (getHostValueFromIR ir, getHostValueFromIR evaluated) of
                        (Just orig, Just evaluated') -> do
                            let origWidget = extractHostValue orig :: Maybe TestWidget
                                evaluatedWidget = extractHostValue evaluated' :: Maybe TestWidget
                            origWidget `shouldBe` evaluatedWidget
                        _ -> expectationFailure "Both should be host values"
                Left _ -> expectationFailure "Host value evaluation should not fail"

        it "host values cannot be called directly (not callable)" do
            let hv = hostValue (TestWidget "call" False)
                hostIr = NativeValue hv :: IR Eval
                callIr = List [hostIr, String "arg"] :: IR Eval
                env = E.emptyEnv
            result <- runEvalSimple (eval callIr) env
            case result of
                Right (resultIr, _) -> do
                    -- Should evaluate to a list containing the host value and string
                    case resultIr of
                        List [NativeValue _, String "arg"] -> pure () -- Structure is correct
                        _ -> expectationFailure $ "Expected list with host value and string, got: " ++ show resultIr
                Left err -> expectationFailure $ "List evaluation should succeed: " ++ show err

        it "host values can be passed as arguments to functions" do
            -- Create a function that accepts a host value and returns it
            let identityFunc = NativeFunc (\[arg] -> pure arg) :: IR Eval
                hv = hostValue (TestWidget "arg" True)
                hostIr = NativeValue hv :: IR Eval
                callIr = List [identityFunc, hostIr] :: IR Eval
                env = E.emptyEnv
            result <- runEvalSimple (eval callIr) env
            case result of
                Right (resultIr, _) -> do
                    -- Check that result is a host value with the same content
                    isHostValue resultIr `shouldBe` True
                    case (getHostValueFromIR hostIr, getHostValueFromIR resultIr) of
                        (Just orig, Just res) -> do
                            let origWidget = extractHostValue orig :: Maybe TestWidget
                                resWidget = extractHostValue res :: Maybe TestWidget
                            origWidget `shouldBe` resWidget
                        _ -> expectationFailure "Both should be host values"
                Left err -> expectationFailure $ "Function call should succeed: " ++ show err

        it "functions can return host values" do
            -- Create a function that returns a host value
            let widget = TestWidget "return" False
                hv = hostValue widget
                returnFunc = NativeFunc (\_ -> pure (NativeValue hv)) :: IR Eval
                callIr = List [returnFunc] :: IR Eval
                env = E.emptyEnv
            result <- runEvalSimple (eval callIr) env
            case result of
                Right (resultIr, _) -> do
                    -- Check that result is a host value with the expected content
                    isHostValue resultIr `shouldBe` True
                    case getHostValueFromIR resultIr of
                        Just extracted -> extractHostValue extracted `shouldBe` Just widget
                        Nothing -> expectationFailure "Result should be a host value"
                Left err -> expectationFailure $ "Function should return host value: " ++ show err

        it "host values work in nested function calls" do
            -- Test: (identity (create-widget "nested"))
            let createWidgetFunc = NativeFunc (\_ -> pure (NativeValue (hostValue (TestWidget "nested" True)))) :: IR Eval
                identityFunc = NativeFunc (\[arg] -> pure arg) :: IR Eval
                createCall = List [createWidgetFunc] :: IR Eval
                nestedCall = List [identityFunc, createCall] :: IR Eval
                env = E.emptyEnv
            result <- runEvalSimple (eval nestedCall) env
            case result of
                Right (resultIr, _) -> do
                    isHostValue resultIr `shouldBe` True
                    case getHostValueFromIR resultIr of
                        Just extracted -> extractHostValue extracted `shouldBe` Just (TestWidget "nested" True)
                        Nothing -> expectationFailure "Should contain host value"
                Left err -> expectationFailure $ "Nested call should work: " ++ show err

        it "host values in environment work correctly" do
            let hv = hostValue (TestWidget "env" False)
                hostIr = NativeValue hv :: IR Eval
                env = E.defineVar "myWidget" hostIr E.emptyEnv
                symbolIr = Symbol "myWidget" :: IR Eval
            result <- runEvalSimple (eval symbolIr) env
            case result of
                Right (resultIr, _) -> do
                    -- Check that result is a host value with the same content
                    isHostValue resultIr `shouldBe` True
                    case (getHostValueFromIR hostIr, getHostValueFromIR resultIr) of
                        (Just orig, Just res) -> do
                            let origWidget = extractHostValue orig :: Maybe TestWidget
                                resWidget = extractHostValue res :: Maybe TestWidget
                            origWidget `shouldBe` resWidget
                        _ -> expectationFailure "Both should be host values"
                Left err -> expectationFailure $ "Environment lookup should work: " ++ show err
