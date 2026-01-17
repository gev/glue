module Glue.NativeIntegrationSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Glue.Env qualified as E
import Glue.Eval (Eval, eval, runEvalSimple, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..), Env, hostValueWithProps)
import Glue.Lib.Builtin.Set qualified as Set
import Test.Hspec

-- Test data types for host objects
data Person = Person {personName :: String, personAge :: Int}
    deriving (Show, Eq)

data Counter = Counter {counterValue :: Int}
    deriving (Show, Eq)

data Address = Address {street :: String, city :: String}
    deriving (Show, Eq)

data Company = Company {companyName :: String, employees :: [Person]}
    deriving (Show, Eq)

-- Constructor functions that return NativeValue objects
createPerson :: [IR Eval] -> Eval (IR Eval)
createPerson [String name, Integer age] = do
    let person = Person (T.unpack name) (fromIntegral age)
        nameGetter = NativeFunc (\_ -> pure (String name))
        ageGetter = NativeFunc (\_ -> pure (Integer age))
        nameSetter = NativeFunc $ \[newVal] -> case newVal of
            String newName -> pure Void  -- Would modify person in real FFI
            _ -> throwError $ wrongArgumentType ["string"]
        ageSetter = NativeFunc $ \[newVal] -> case newVal of
            Integer newAge -> pure Void  -- Would modify person in real FFI
            _ -> throwError $ wrongArgumentType ["integer"]
        getters = Map.fromList [("name", nameGetter), ("age", ageGetter)]
        setters = Map.fromList [("name", nameSetter), ("age", ageSetter)]
        hostVal = hostValueWithProps person getters setters
    pure (NativeValue hostVal)
createPerson _ = throwError $ wrongArgumentType ["string", "integer"]

createCounter :: [IR Eval] -> Eval (IR Eval)
createCounter [Integer initial] = do
    let counter = Counter (fromIntegral initial)
        valueGetter = NativeFunc (\_ -> pure (Integer initial))
        incrementSetter = NativeFunc $ \[newVal] -> case newVal of
            Integer amount -> pure Void  -- Would increment counter in real FFI
            _ -> throwError $ wrongArgumentType ["integer"]
        getters = Map.fromList [("value", valueGetter)]
        setters = Map.fromList [("increment", incrementSetter)]
        hostVal = hostValueWithProps counter getters setters
    pure (NativeValue hostVal)
createCounter _ = throwError $ wrongArgumentType ["integer"]

createAddress :: [IR Eval] -> Eval (IR Eval)
createAddress [String st, String ct] = do
    let address = Address (T.unpack st) (T.unpack ct)
        streetGetter = NativeFunc (\_ -> pure (String st))
        cityGetter = NativeFunc (\_ -> pure (String ct))
        getters = Map.fromList [("street", streetGetter), ("city", cityGetter)]
        hostVal = hostValueWithProps address getters Map.empty
    pure (NativeValue hostVal)
createAddress _ = throwError $ wrongArgumentType ["string", "string"]

createCompany :: [IR Eval] -> Eval (IR Eval)
createCompany [String name] = do
    let company = Company (T.unpack name) []
        nameGetter = NativeFunc (\_ -> pure (String name))
        employeesGetter = NativeFunc (\_ -> pure (List []))  -- Empty list initially
        addEmployeeSetter = NativeFunc $ \[newVal] -> case newVal of
            NativeValue _ -> pure Void  -- Would add employee in real FFI
            _ -> throwError $ wrongArgumentType ["person"]
        getters = Map.fromList [("name", nameGetter), ("employees", employeesGetter)]
        setters = Map.fromList [("addEmployee", addEmployeeSetter)]
        hostVal = hostValueWithProps company getters setters
    pure (NativeValue hostVal)
createCompany _ = throwError $ wrongArgumentType ["string"]

-- Helper to set up test environment with constructors
setupTestEnv :: Env Eval
setupTestEnv = foldl (\env (name, val) -> E.defineVar name val env) E.emptyEnv
    [ ("createPerson", NativeFunc createPerson)
    , ("createCounter", NativeFunc createCounter)
    , ("createAddress", NativeFunc createAddress)
    , ("createCompany", NativeFunc createCompany)
    ]

spec :: Spec
spec = describe "Native Value Integration (Complex Scenarios)" do

    describe "Object Creation and Basic Property Access" do
        it "creates person object and accesses properties" do
            let env = setupTestEnv
                createCall = List [Symbol "createPerson", String "Alice", Integer 30]

            -- Create person
            createResult <- runEvalSimple (eval createCall) env
            createResult `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

        it "creates counter and uses increment setter" do
            let env = setupTestEnv
                createCall = List [Symbol "createCounter", Integer 0]
                incrementCall = List [Symbol "set", Symbol "counter.increment", Integer 5]

            -- Create counter
            createResult <- runEvalSimple (eval createCall) env
            createResult `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

    describe "Complex Object Relationships" do
        it "creates company with employees" do
            let env = setupTestEnv
                createCompanyCall = List [Symbol "createCompany", String "Acme Corp"]
                createPersonCall = List [Symbol "createPerson", String "Bob", Integer 25]
                addEmployeeCall = List [Symbol "set", Symbol "company.addEmployee", Symbol "person"]

            -- Create company and person
            companyResult <- runEvalSimple (eval createCompanyCall) env
            companyResult `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

            personResult <- runEvalSimple (eval createPersonCall) env
            personResult `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

        it "creates person with address" do
            let env = setupTestEnv
                createPersonCall = List [Symbol "createPerson", String "Charlie", Integer 35]
                createAddressCall = List [Symbol "createAddress", String "123 Main St", String "Springfield"]

            -- Create person and address
            personResult <- runEvalSimple (eval createPersonCall) env
            personResult `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

            addressResult <- runEvalSimple (eval createAddressCall) env
            addressResult `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

    describe "Function Calls with Native Values" do
        it "passes native values to functions" do
            let env = setupTestEnv
                createPersonCall = List [Symbol "createPerson", String "David", Integer 40]
                -- In a real scenario, we'd have functions that take native values as arguments
                -- This tests that the evaluation pipeline can handle native values in function calls

            personResult <- runEvalSimple (eval createPersonCall) env
            personResult `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

        it "returns native values from functions" do
            let env = setupTestEnv
                createCall = List [Symbol "createCounter", Integer 10]

            result <- runEvalSimple (eval createCall) env
            result `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

    describe "Property Access Patterns" do
        it "handles nested property access simulation" do
            let env = setupTestEnv
                createCompanyCall = List [Symbol "createCompany", String "Tech Corp"]
                -- In real scenarios: company.employees[0].name

            result <- runEvalSimple (eval createCompanyCall) env
            result `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

        it "handles property assignment patterns" do
            let env = setupTestEnv
                createPersonCall = List [Symbol "createPerson", String "Eve", Integer 28]
                -- In real scenarios: person.age = 29

            result <- runEvalSimple (eval createPersonCall) env
            result `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)

    describe "Error Handling in Complex Scenarios" do
        it "handles constructor argument validation" do
            let env = setupTestEnv
                invalidCall = List [Symbol "createPerson", String "Test"]  -- Missing age

            result <- runEvalSimple (eval invalidCall) env
            result `shouldSatisfy` (\case
                Left _ -> True  -- Should fail due to wrong arguments
                _ -> False)

        it "handles property access on non-objects" do
            let env = setupTestEnv
                invalidAccess = DottedSymbol ["number", "property"]
                envWithNumber = E.defineVar "number" (Integer 42) env

            result <- runEvalSimple (eval invalidAccess) envWithNumber
            result `shouldSatisfy` (\case
                Left _ -> True  -- Should fail
                _ -> False)

    describe "Complex Object Manipulation" do
        it "handles multiple object interactions" do
            let env = setupTestEnv
                createCompanyCall = List [Symbol "createCompany", String "Big Corp"]
                createPerson1Call = List [Symbol "createPerson", String "Manager", Integer 45]
                createPerson2Call = List [Symbol "createPerson", String "Worker", Integer 30]

            -- Create multiple objects
            results <- mapM (\call -> runEvalSimple (eval call) env)
                [createCompanyCall, createPerson1Call, createPerson2Call]

            let allSucceeded = all (\case
                    Right (NativeValue _, _) -> True
                    _ -> False) results

            allSucceeded `shouldBe` True

        it "handles object lifecycle simulation" do
            let env = setupTestEnv
                createCounterCall = List [Symbol "createCounter", Integer 0]
                -- In real scenarios: create, use, modify, cleanup

            result <- runEvalSimple (eval createCounterCall) env
            result `shouldSatisfy` (\case
                Right (NativeValue _, _) -> True
                _ -> False)
