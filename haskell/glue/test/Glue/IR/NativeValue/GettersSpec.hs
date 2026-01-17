{-# OPTIONS_GHC -Wno-orphans #-}

module Glue.IR.NativeValue.GettersSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (Eval, eval, runEvalSimple)
import Glue.IR (IR (..), extractHostValue, getHostValueFromIR, hostValue)
import Test.Hspec

-- Test data types for host values
data TestWidget = TestWidget {label :: String, enabled :: Bool}
    deriving (Show, Eq)

spec :: Spec
spec = describe "HostValue property getters" do
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
