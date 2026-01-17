module Glue.IR.NativeValue.SettersSpec (spec) where

import Data.Map.Strict qualified as Map
import Glue.Env qualified as E
import Glue.Eval (Eval, runEvalSimple, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..), hostValueWithProps)
import Glue.Lib.Builtin.Set qualified as Set
import Test.Hspec

spec :: Spec
spec = describe "HostValue property setters" do
    describe "Property setting with type checking" do
        it "sets string property with correct type" do
            -- Create a setter that validates string type
            let nameSetter = \newVal -> case newVal of
                    String _ -> pure Void -- Accept any string
                    _ -> throwError $ wrongArgumentType ["string"]
                setters = Map.fromList [("name", nameSetter)]
                hostVal = hostValueWithProps () Map.empty setters
                hostIr = NativeValue hostVal

            -- Set up environment and call set function
            let env = E.defineVar "obj" hostIr E.emptyEnv
                setArgs = [Symbol "obj.name", String "Alice"]

            result <- runEvalSimple (Set.set setArgs) env
            case result of
                Right (Void, _) -> pure () -- Success
                Left err -> expectationFailure $ "Property setting should succeed: " ++ show err

        it "sets numeric property with correct type" do
            -- Create a setter that validates integer type
            let sizeSetter = \newVal -> case newVal of
                    Integer _ -> pure Void -- Accept any integer
                    _ -> throwError $ wrongArgumentType ["integer"]
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

    describe "Type checking and error handling" do
        it "throws error when setting string property to wrong type" do
            -- Create a setter that only accepts strings
            let nameSetter = \newVal -> case newVal of
                    String _ -> pure Void
                    _ -> throwError $ wrongArgumentType ["string"]
                setters = Map.fromList [("name", nameSetter)]
                hostVal = hostValueWithProps () Map.empty setters
                hostIr = NativeValue hostVal

            -- Try to set name to an integer (wrong type)
            let env = E.defineVar "obj" hostIr E.emptyEnv
                setArgs = [Symbol "obj.name", Integer 123]

            result <- runEvalSimple (Set.set setArgs) env
            case result of
                Left _ -> pure () -- Should fail with type error
                Right (val, _) -> expectationFailure $ "Setting string property to integer should fail, but got: " ++ show val

        it "throws error when setting numeric property to wrong type" do
            -- Create a setter that only accepts integers
            let ageSetter = \newVal -> case newVal of
                    Integer _ -> pure Void
                    _ -> throwError $ wrongArgumentType ["integer"]
                setters = Map.fromList [("age", ageSetter)]
                hostVal = hostValueWithProps () Map.empty setters
                hostIr = NativeValue hostVal

            -- Try to set age to a string (wrong type)
            let env = E.defineVar "obj" hostIr E.emptyEnv
                setArgs = [Symbol "obj.age", String "thirty"]

            result <- runEvalSimple (Set.set setArgs) env
            case result of
                Left _ -> pure () -- Should fail with type error
                Right (val, _) -> expectationFailure $ "Setting numeric property to string should fail, but got: " ++ show val

    describe "Multiple properties with different types" do
        it "sets different typed properties on the same object" do
            let nameSetter = \newVal -> case newVal of
                    String _ -> pure Void
                    _ -> throwError $ wrongArgumentType ["string"]
                ageSetter = \newVal -> case newVal of
                    Integer _ -> pure Void
                    _ -> throwError $ wrongArgumentType ["integer"]
                setters = Map.fromList [("name", nameSetter), ("age", ageSetter)]
                hostVal = hostValueWithProps () Map.empty setters
                hostIr = NativeValue hostVal

            let env = E.defineVar "obj" hostIr E.emptyEnv

            -- Set name (string)
            let setNameArgs = [Symbol "obj.name", String "Bob"]
            result1 <- runEvalSimple (Set.set setNameArgs) env
            case result1 of
                Right (Void, _) -> pure ()
                Left err -> expectationFailure $ "Name setting should succeed: " ++ show err

            -- Set age (integer)
            let setAgeArgs = [Symbol "obj.age", Integer 25]
            result2 <- runEvalSimple (Set.set setAgeArgs) env
            case result2 of
                Right (Void, _) -> pure ()
                Left err -> expectationFailure $ "Age setting should succeed: " ++ show err

    describe "Error handling for missing properties" do
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
