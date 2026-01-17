module Glue.IR.NativeValue.SettersSpec (spec) where

import Data.Map.Strict qualified as Map
import Glue.Env qualified as E
import Glue.Eval (Eval, runEvalSimple)
import Glue.IR (IR (..), hostValueWithProps)
import Glue.Lib.Builtin.Set qualified as Set
import Test.Hspec

spec :: Spec
spec = describe "HostValue property setters" do
    describe "Property setting with setter functions" do
        it "sets property using setter function" do
            -- Create a setter function that just returns Void (simulating a side effect)
            let labelSetter = NativeFunc (\[String _newLabel] -> pure Void)
                setters = Map.fromList [("label", labelSetter)]
                hostVal = hostValueWithProps () Map.empty setters
                hostIr = NativeValue hostVal

            -- Set up environment and call set function
            let env = E.defineVar "obj" hostIr E.emptyEnv
                setArgs = [Symbol "obj.label", String "updated"]

            result <- runEvalSimple (Set.set setArgs) env
            case result of
                Right (Void, _) -> pure () -- Success - setter was called and returned Void
                Left err -> expectationFailure $ "Property setting should succeed: " ++ show err

        it "sets numeric property using setter function" do
            -- Create a setter that accepts an integer
            let sizeSetter = NativeFunc (\[Integer _newSize] -> pure Void)
                setters = Map.fromList [("size", sizeSetter)]
                hostVal = hostValueWithProps () Map.empty setters
                hostIr = NativeValue hostVal

            -- Set up environment and call set function
            let env = E.defineVar "obj" hostIr E.emptyEnv
                setArgs = [Symbol "obj.size", Integer 100]

            result <- runEvalSimple (Set.set setArgs) env
            case result of
                Right (Void, _) -> pure () -- Success
                Left err -> expectationFailure $ "Property setting should succeed: " ++ show err

    describe "Multiple setters on same object" do
        it "sets different properties on the same host value" do
            let labelSetter = NativeFunc (\[String _newLabel] -> pure Void)
                enabledSetter = NativeFunc (\[Bool _newEnabled] -> pure Void)
                setters = Map.fromList [("label", labelSetter), ("enabled", enabledSetter)]
                hostVal = hostValueWithProps () Map.empty setters
                hostIr = NativeValue hostVal

            let env = E.defineVar "obj" hostIr E.emptyEnv

            -- Set label
            let setLabelArgs = [Symbol "obj.label", String "changed"]
            result1 <- runEvalSimple (Set.set setLabelArgs) env
            case result1 of
                Right (Void, _) -> pure ()
                Left err -> expectationFailure $ "Label setting should succeed: " ++ show err

            -- Set enabled
            let setEnabledArgs = [Symbol "obj.enabled", Bool True]
            result2 <- runEvalSimple (Set.set setEnabledArgs) env
            case result2 of
                Right (Void, _) -> pure ()
                Left err -> expectationFailure $ "Enabled setting should succeed: " ++ show err

    describe "Error handling" do
        it "fails when setting non-existent property" do
            let hostVal = hostValueWithProps () Map.empty Map.empty
                hostIr = NativeValue hostVal
                env = E.defineVar "obj" hostIr E.emptyEnv
                setArgs = [Symbol "obj.nonexistent", String "value"]

            result <- runEvalSimple (Set.set setArgs) env
            case result of
                Left _ -> pure () -- Should fail
                Right (val, _) -> expectationFailure $ "Setting non-existent property should fail, but got: " ++ show val

        it "fails when setting property on non-host value" do
            let env = E.defineVar "number" (Integer 42) E.emptyEnv
                setArgs = [Symbol "number.property", String "value"]

            result <- runEvalSimple (Set.set setArgs) env
            case result of
                Left _ -> pure () -- Should fail
                Right (val, _) -> expectationFailure $ "Setting property on non-host value should fail, but got: " ++ show val
