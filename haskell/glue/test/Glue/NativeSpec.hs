module Glue.NativeSpec (spec) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Glue.Env qualified as E
import Glue.Eval (Eval, liftIO, runEvalSimple, throwError)
import Glue.Eval qualified
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (Env, IR (..), extractHostValue, hostValueWithProps)
import Glue.IR qualified
import Glue.Lib.Builtin.Def (def)
import Glue.Lib.Builtin.Set qualified as Set
import Glue.Parser qualified
import Test.Hspec

-- Test data types for host objects with mutable state
data Person = Person
    { personName :: IORef Text
    , personAge :: IORef Int
    , personAddress :: IORef (Maybe (IR Eval))
    }

data Address = Address
    { addressStreet :: IORef Text
    , addressCity :: IORef Text
    }

-- Constructor functions that take object literals and create native objects
person :: [IR Eval] -> Eval (IR Eval)
person [Object props] = do
    -- Extract properties from object literal with type checking
    name <- case Map.lookup "name" props of
        Just (String n) -> pure n
        _ -> throwError $ wrongArgumentType ["name: string"]

    age <- case Map.lookup "age" props of
        Just (Integer a) -> pure (fromIntegral a)
        _ -> throwError $ wrongArgumentType ["age: integer"]

    address <- case Map.lookup "address" props of
        Just (NativeValue addrHostValue) ->
            case extractHostValue addrHostValue :: Maybe Address of
                Just _ -> pure $ Just (NativeValue addrHostValue) -- Store the NativeValue
                Nothing -> throwError $ wrongArgumentType ["address: Address"]
        Just _ -> throwError $ wrongArgumentType ["address: Address"]
        Nothing -> pure Nothing -- Address is optional

    -- Create mutable Person object
    nameRef <- liftIO $ newIORef name
    ageRef <- liftIO $ newIORef age
    addressRef <- liftIO $ newIORef address

    let personObj = Person nameRef ageRef addressRef

    -- Create getters and setters
    let nameGetter = do
            currentName <- liftIO $ readIORef nameRef
            pure (String currentName)
        ageGetter = do
            currentAge <- liftIO $ readIORef ageRef
            pure (Integer $ fromIntegral currentAge)
        addressGetter = do
            currentAddr <- liftIO $ readIORef addressRef
            case currentAddr of
                Just addrNativeValue -> pure addrNativeValue
                Nothing -> pure (String "no address")
        nameSetter = \case
            String newName -> liftIO $ writeIORef nameRef newName >> pure Void
            _ -> throwError $ wrongArgumentType ["string"]
        ageSetter = \case
            Integer newAge -> liftIO $ writeIORef ageRef (fromIntegral newAge) >> pure Void
            _ -> throwError $ wrongArgumentType ["integer"]
        addressSetter = \case
            NativeValue addrHostValue ->
                case extractHostValue addrHostValue :: Maybe Address of
                    Just _ -> liftIO $ writeIORef addressRef (Just (NativeValue addrHostValue)) >> pure Void
                    Nothing -> throwError $ wrongArgumentType ["address: Address"]
            _ -> throwError $ wrongArgumentType ["NativeValue"]

    let getters =
            Map.fromList
                [ ("name", nameGetter)
                , ("age", ageGetter)
                , ("address", addressGetter)
                ]
        setters =
            Map.fromList
                [ ("name", nameSetter)
                , ("age", ageSetter)
                , ("address", addressSetter)
                ]

    pure (NativeValue $ hostValueWithProps personObj getters setters)
person _ = throwError $ wrongArgumentType ["object"]

address :: [IR Eval] -> Eval (IR Eval)
address [Object props] = do
    -- Extract properties from object literal with type checking
    street <- case Map.lookup "street" props of
        Just (String s) -> pure s
        _ -> throwError $ wrongArgumentType ["street: string"]

    city <- case Map.lookup "city" props of
        Just (String c) -> pure c
        _ -> throwError $ wrongArgumentType ["city: string"]

    -- Create mutable Address object
    streetRef <- liftIO $ newIORef street
    cityRef <- liftIO $ newIORef city

    let addrObj = Address streetRef cityRef

    -- Create getters and setters
    let streetGetter = do
            currentStreet <- liftIO $ readIORef streetRef
            pure (String currentStreet)
        cityGetter = do
            currentCity <- liftIO $ readIORef cityRef
            pure (String currentCity)
        streetSetter = \case
            String newStreet -> liftIO $ writeIORef streetRef newStreet >> pure Void
            _ -> throwError $ wrongArgumentType ["string"]
        citySetter = \case
            String newCity -> liftIO $ writeIORef cityRef newCity >> pure Void
            _ -> throwError $ wrongArgumentType ["string"]

    let getters =
            Map.fromList
                [ ("street", streetGetter)
                , ("city", cityGetter)
                ]
        setters =
            Map.fromList
                [ ("street", streetSetter)
                , ("city", citySetter)
                ]

    pure (NativeValue $ hostValueWithProps addrObj getters setters)
address _ = throwError $ wrongArgumentType ["object"]

-- Test environment with constructors
testEnv :: Env Eval
testEnv =
    foldl
        (\env (name, val) -> E.defineVar name val env)
        E.emptyEnv
        [ ("def", Special def)
        , ("set", Special Set.set)
        , ("person", NativeFunc person)
        , ("address", NativeFunc address)
        ]

-- Helper to run Glue code
runGlueCode :: T.Text -> IO (Either String (IR Eval))
runGlueCode input = case Glue.Parser.parseGlue input of
    Left err -> pure $ Left $ "Parse error: " ++ show err
    Right ast -> do
        let irTree = Glue.IR.compile ast
        fullResult <- runEvalSimple (Glue.Eval.eval irTree) testEnv
        case fullResult of
            Left err -> pure $ Left $ "Eval error: " ++ show err
            Right (res, _) -> case res of
                List [] -> pure $ Right Void
                List xs -> pure $ Right (last xs)
                other -> pure $ Right other

spec :: Spec
spec = describe "Full FFI Integration Tests" do
    describe "Basic Object Creation and Property Access" do
        it "creates person and accesses properties" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def bob (person :name \"Bob\" :age 25))"
                        , "  bob.name"
                        , ")"
                        ]
            result `shouldBe` Right (String "Bob")

        it "creates address and accesses properties" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def addr (address :street \"123 Main St\" :city \"Springfield\"))"
                        , "  addr.street"
                        , ")"
                        ]
            result `shouldBe` Right (String "123 Main St")

    describe "Property Modification" do
        it "modifies person properties" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def bob (person :name \"Bob\" :age 25))"
                        , "  (set bob.age 26)"
                        , "  bob.age"
                        , ")"
                        ]
            result `shouldBe` Right (Integer 26)

        it "modifies address properties" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def addr (address :street \"123 Main St\" :city \"Springfield\"))"
                        , "  (set addr.city \"Boston\")"
                        , "  addr.city"
                        , ")"
                        ]
            result `shouldBe` Right (String "Boston")

    describe "Complex Object Relationships" do
        it "creates person with address" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def addr (address :street \"123 Main St\" :city \"Springfield\"))"
                        , "  (def bob (person :name \"Bob\" :age 25 :address addr))"
                        , "  bob.address.city"
                        , ")"
                        ]
            result `shouldBe` Right (String "Springfield")

        it "modifies nested properties" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def addr (address :street \"123 Main St\" :city \"Springfield\"))"
                        , "  (def bob (person :name \"Bob\" :age 25 :address addr))"
                        , "  (set bob.address.city \"Boston\")"
                        , "  bob.address.city"
                        , ")"
                        ]
            result `shouldBe` Right (String "Boston")

    describe "Multiple Operations in Sequence" do
        it "performs complex object manipulation" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def addr (address :street \"123 Main St\" :city \"Springfield\"))"
                        , "  (def bob (person :name \"Bob\" :age 25 :address addr))"
                        , "  (set bob.age 26)"
                        , "  (set bob.name \"Robert\")"
                        , "  (set bob.address.city \"Boston\")"
                        , "  (set bob.address.street \"456 Oak Ave\")"
                        , "  bob.name"
                        , ")"
                        ]
            result `shouldBe` Right (String "Robert")

        it "verifies all modifications persist" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def addr (address :street \"123 Main St\" :city \"Springfield\"))"
                        , "  (def bob (person :name \"Bob\" :age 25 :address addr))"
                        , "  (set bob.age 26)"
                        , "  (set bob.address.city \"Boston\")"
                        , "  bob.age"
                        , ")"
                        ]
            result `shouldBe` Right (Integer 26)

        it "sets new address on person" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def addr1 (address :street \"123 Main St\" :city \"Springfield\"))"
                        , "  (def addr2 (address :street \"456 Oak Ave\" :city \"Boston\"))"
                        , "  (def bob (person :name \"Bob\" :age 25 :address addr1))"
                        , "  (set bob.address addr2)"
                        , "  bob.address.city"
                        , ")"
                        ]
            result `shouldBe` Right (String "Boston")

    describe "Error Handling" do
        it "fails with wrong constructor arguments" $ do
            result <- runGlueCode "(person \"Bob\")" -- Missing object
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with wrong name type" $ do
            result <- runGlueCode "(person :name 123 :age 25)" -- name should be string
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with wrong age type" $ do
            result <- runGlueCode "(person :name \"Bob\" :age \"25\")" -- age should be integer
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with wrong address type" $ do
            result <- runGlueCode "(person :name \"Bob\" :age 25 :address \"not-an-address\")" -- address should be Address
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with missing name field" $ do
            result <- runGlueCode "(person :age 25)" -- name is required
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with missing age field" $ do
            result <- runGlueCode "(person :name \"Bob\")" -- age is required
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with wrong street type" $ do
            result <- runGlueCode "(address :street 123 :city \"Springfield\")" -- street should be string
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with wrong city type" $ do
            result <- runGlueCode "(address :street \"123 Main St\" :city 456)" -- city should be string
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with missing street field" $ do
            result <- runGlueCode "(address :city \"Springfield\")" -- street is required
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails with missing city field" $ do
            result <- runGlueCode "(address :street \"123 Main St\")" -- city is required
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails accessing non-existent properties" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def bob (person :name \"Bob\" :age 25))"
                        , "  bob.nonexistent"
                        , ")"
                        ]
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )

        it "fails setting wrong types" $ do
            result <-
                runGlueCode $
                    T.unlines
                        [ "("
                        , "  (def bob (person :name \"Bob\" :age 25))"
                        , "  (set bob.age \"not-a-number\")"
                        , ")"
                        ]
            result
                `shouldSatisfy` ( \case
                                    Left _ -> True
                                    _ -> False
                                )
