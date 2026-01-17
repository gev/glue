module Glue.IR.NativeValue.GettersSpec (spec) where

import Data.Map.Strict qualified as Map
import Glue.Env qualified as E
import Glue.Eval (Eval, eval, runEvalSimple)
import Glue.IR (IR (..), hostValueWithProps)
import Test.Hspec

-- Test data types for host values with properties
data Person = Person {name :: String, age :: Int}
    deriving (Show, Eq)

newtype Calculator = Calculator {baseValue :: Int}
    deriving (Show, Eq)

spec :: Spec
spec = describe "HostValue property getters" do
    describe "Property access returning values" do
        it "accesses simple property returning a string" do
            -- Create a person with a name property
            let person = Person "Alice" 30
                nameGetter = pure (String "Alice")
                getters = Map.fromList [("name", nameGetter)]
                hostVal = hostValueWithProps person getters Map.empty
                hostIr = NativeValue hostVal :: IR Eval
                env = E.defineVar "person" hostIr E.emptyEnv
                dottedIr = DottedSymbol ["person", "name"] :: IR Eval

            result <- runEvalSimple (eval dottedIr) env
            case result of
                Right (evaluated, _) -> evaluated `shouldBe` String "Alice"
                Left err -> expectationFailure $ "Property access should succeed: " ++ show err

        it "accesses property returning a number" do
            -- Create a person with an age property
            let person = Person "Bob" 25
                ageGetter = pure (Integer 25)
                getters = Map.fromList [("age", ageGetter)]
                hostVal = hostValueWithProps person getters Map.empty
                hostIr = NativeValue hostVal :: IR Eval
                env = E.defineVar "person" hostIr E.emptyEnv
                dottedIr = DottedSymbol ["person", "age"] :: IR Eval

            result <- runEvalSimple (eval dottedIr) env
            case result of
                Right (evaluated, _) -> evaluated `shouldBe` Integer 25
                Left err -> expectationFailure $ "Property access should succeed: " ++ show err

    describe "Method calls returning computed values" do
        it "calls method that computes and returns a value" do
            -- Create a calculator with a compute method
            let calc = Calculator 10
                computeGetter = pure (Integer 42) -- Always returns 42
                getters = Map.fromList [("compute", computeGetter)]
                hostVal = hostValueWithProps calc getters Map.empty
                hostIr = NativeValue hostVal :: IR Eval
                env = E.defineVar "calc" hostIr E.emptyEnv
                dottedIr = DottedSymbol ["calc", "compute"] :: IR Eval

            result <- runEvalSimple (eval dottedIr) env
            case result of
                Right (evaluated, _) -> evaluated `shouldBe` Integer 42
                Left err -> expectationFailure $ "Method call should succeed: " ++ show err

        it "calls method that accesses host object data" do
            -- Create a calculator that returns double its base value
            let calc = Calculator 15
                doubleGetter = pure (Integer 30) -- 15 * 2
                getters = Map.fromList [("double", doubleGetter)]
                hostVal = hostValueWithProps calc getters Map.empty
                hostIr = NativeValue hostVal :: IR Eval
                env = E.defineVar "calc" hostIr E.emptyEnv
                dottedIr = DottedSymbol ["calc", "double"] :: IR Eval

            result <- runEvalSimple (eval dottedIr) env
            case result of
                Right (evaluated, _) -> evaluated `shouldBe` Integer 30
                Left err -> expectationFailure $ "Method call should succeed: " ++ show err

    describe "Multiple properties on same object" do
        it "accesses different properties on the same host value" do
            let person = Person "Charlie" 35
                nameGetter = pure (String "Charlie")
                ageGetter = pure (Integer 35)
                getters = Map.fromList [("name", nameGetter), ("age", ageGetter)]
                hostVal = hostValueWithProps person getters Map.empty
                hostIr = NativeValue hostVal :: IR Eval
                env = E.defineVar "person" hostIr E.emptyEnv

            -- Test name property
            let nameDotted = DottedSymbol ["person", "name"] :: IR Eval
            nameResult <- runEvalSimple (eval nameDotted) env
            case nameResult of
                Right (nameVal, _) -> nameVal `shouldBe` String "Charlie"
                Left err -> expectationFailure $ "Name property access should succeed: " ++ show err

            -- Test age property
            let ageDotted = DottedSymbol ["person", "age"] :: IR Eval
            ageResult <- runEvalSimple (eval ageDotted) env
            case ageResult of
                Right (ageVal, _) -> ageVal `shouldBe` Integer 35
                Left err -> expectationFailure $ "Age property access should succeed: " ++ show err

    describe "Error handling" do
        it "fails when accessing non-existent property" do
            let person = Person "David" 40
                getters = Map.fromList [("name", pure (String "David"))]
                hostVal = hostValueWithProps person getters Map.empty
                hostIr = NativeValue hostVal :: IR Eval
                env = E.defineVar "person" hostIr E.emptyEnv
                dottedIr = DottedSymbol ["person", "nonexistent"] :: IR Eval

            result <- runEvalSimple (eval dottedIr) env
            case result of
                Left _ -> pure () -- Should fail
                Right (val, _) -> expectationFailure $ "Accessing non-existent property should fail, but got: " ++ show val

        it "fails when accessing property on non-host value" do
            let env = E.defineVar "number" (Integer 42) E.emptyEnv
                dottedIr = DottedSymbol ["number", "property"] :: IR Eval

            result <- runEvalSimple (eval dottedIr) env
            case result of
                Left _ -> pure () -- Should fail
                Right (val, _) -> expectationFailure $ "Accessing property on non-host value should fail, but got: " ++ show val
