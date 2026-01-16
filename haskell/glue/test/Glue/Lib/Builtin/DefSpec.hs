module Glue.Lib.Builtin.DefSpec (spec) where

import Data.Either (isLeft)
import Glue.Env qualified as E
import Glue.Eval (Runtime (..), runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Builtin (builtin)
import Glue.Lib.Builtin.Def (def)
import Glue.Module (envFromModules)
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Builtin.Def (Test def special form)" do
    describe "Defining variables" do
        it "defines a variable in the environment" do
            let args = [Symbol "x", Integer 42]
            let env = envFromModules [builtin]
            result <- runEvalSimple (def args) env
            case result of
                Left err -> expectationFailure $ "Def failed: " <> show err
                Right (res, runtime) -> do
                    res `shouldBe` Void
                    E.lookupLocal "x" runtime.env `shouldBe` Just (Integer 42)

        it "fails with wrong number of arguments" do
            let args = [Symbol "x"]
            let env = envFromModules [builtin]
            result <- runEvalSimple (def args) env
            result `shouldSatisfy` isLeft

        it "fails with non-symbol as name" do
            let args = [Integer 1, Integer 42]
            let env = envFromModules [builtin]
            result <- runEvalSimple (def args) env
            result `shouldSatisfy` isLeft

    describe "Function definition sugar" do
        it "defines simple function" do
            let args = [List [Symbol "square", Symbol "x"], List [Symbol "*", Symbol "x", Symbol "x"]]
            let env = envFromModules [builtin]
            result <- runEvalSimple (def args) env
            case result of
                Left err -> expectationFailure $ "Def failed: " <> show err
                Right (res, runtime) -> do
                    -- Should return the closure
                    res
                        `shouldSatisfy` ( \case
                                            Closure ["x"] _ _ -> True
                                            _ -> False
                                        )
                    -- Check that square function was also defined
                    E.lookupLocal "square" runtime.env
                        `shouldSatisfy` ( \case
                                            Just (Closure ["x"] _ _) -> True
                                            _ -> False
                                        )

        it "defines function with multiple parameters" do
            let args = [List [Symbol "add", Symbol "x", Symbol "y"], List [Symbol "+", Symbol "x", Symbol "y"]]
            let env = envFromModules [builtin]
            result <- runEvalSimple (def args) env
            case result of
                Left err -> expectationFailure $ "Def failed: " <> show err
                Right (res, runtime) -> do
                    -- Should return the closure
                    res
                        `shouldSatisfy` ( \case
                                            Closure ["x", "y"] _ _ -> True
                                            _ -> False
                                        )
                    E.lookupLocal "add" runtime.env
                        `shouldSatisfy` ( \case
                                            Just (Closure ["x", "y"] _ _) -> True
                                            _ -> False
                                        )

        it "defines function with multiple body expressions" do
            let args =
                    [ List [Symbol "test", Symbol "x"]
                    , List [Symbol "println", String "hello"]
                    , List [Symbol "*", Symbol "x", Integer 2]
                    ]
            let env = envFromModules [builtin]
            result <- runEvalSimple (def args) env
            case result of
                Left err -> expectationFailure $ "Def failed: " <> show err
                Right (res, runtime) -> do
                    -- Should return the closure
                    res
                        `shouldSatisfy` ( \case
                                            Closure ["x"] _ _ -> True
                                            _ -> False
                                        )
                    E.lookupLocal "test" runtime.env
                        `shouldSatisfy` ( \case
                                            Just (Closure ["x"] _ _) -> True
                                            _ -> False
                                        )

        it "fails with invalid function signature" do
            let args = [List [Integer 42, Symbol "x"], List [Symbol "*", Symbol "x", Symbol "x"]]
            let env = envFromModules [builtin]
            result <- runEvalSimple (def args) env
            result `shouldSatisfy` isLeft
