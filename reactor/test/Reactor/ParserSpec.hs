module Reactor.ParserSpec (spec) where

import Reactor.AST
import Reactor.Error (ReactorError (..))
import Reactor.Parser (parseReactor)
import Test.Hspec

spec :: Spec
spec = do
    describe "Reactor LISP Parser" $ do
        describe "Basic Atoms" $ do
            it "parses integers" $ do
                parseReactor "123" `shouldBe` Right (RNumber 123)

            it "parses strings" $ do
                parseReactor "\"hello\"" `shouldBe` Right (RString "hello")

            it "parses symbols" $ do
                parseReactor "void" `shouldBe` Right (RSymbol "void")
                parseReactor "my-func" `shouldBe` Right (RSymbol "my-func")

        describe "Rule: No Mixed Content" $ do
            it "successfully parses pure positional list" $ do
                let input = "(1 2 \"test\")"
                case parseReactor input of
                    Right (RList (RAtoms [RNumber 1, RNumber 2, RString "test"])) -> pure ()
                    _ -> expectationFailure "Should be RAtoms"

            it "successfully parses pure property list" $ do
                let input = "(:id 1 :type \"lamp\")"
                case parseReactor input of
                    Right (RList (RProps [("id", RNumber 1), ("type", RString "lamp")])) -> pure ()
                    _ -> expectationFailure "Should be RProps"

            it "FAILS when mixing atoms and properties" $ do
                -- Прямое сравнение с конструктором ошибки
                parseReactor "(:id 1 \"oops\")" `shouldBe` Left (MixedContent "\"oops\"")

            it "FAILS when mixing properties and atoms" $ do
                parseReactor "(1 2 :id 3)" `shouldBe` Left (MixedContent ":id")

        describe "Rule: Property Pairs" $ do
            it "FAILS on unpaired property key" $ do
                parseReactor "(:id 1 :status)" `shouldBe` Left (UnpairedProperty ":status")

        describe "Expressions (RExpr)" $ do
            it "parses (void ...) as RExpr" $ do
                case parseReactor "(void :key 1)" of
                    Right (RExpr "void" (RProps [("key", RNumber 1)])) -> pure ()
                    _ -> expectationFailure "Should parse as RExpr with name 'void'"

        describe "Syntax Errors" $ do
            it "wraps Megaparsec errors into SyntaxError" $ do
                case parseReactor "(unclosed list" of
                    Left (SyntaxError _) -> pure ()
                    _ -> expectationFailure "Expected a SyntaxError for unclosed parenthesis"

        describe "Quote sugar" $ do
            it "parses quoted symbols as (quote symbol)" $ do
                parseReactor "'foo" `shouldBe` Right (RExpr "quote" (RAtoms [RSymbol "foo"]))

            it "parses quoted lists" $ do
                parseReactor "'(1 2)" `shouldBe` Right (RExpr "quote" (RAtoms [RList (RAtoms [RNumber 1, RNumber 2])]))

        describe "Advanced Quote sugar" do
            it "parses nested quotes (quote of quote)" do
                -- ''foo -> (quote (quote foo))
                parseReactor "''foo"
                    `shouldBe` Right
                        (RExpr "quote" (RAtoms [RExpr "quote" (RAtoms [RSymbol "foo"])]))

            it "parses quote inside a list" do
                -- (list 'a 1) -> (list (quote a) 1)
                parseReactor "(list 'a 1)"
                    `shouldBe` Right
                        (RExpr "list" (RAtoms [RExpr "quote" (RAtoms [RSymbol "a"]), RNumber 1]))

            it "parses quote of a list with properties" do
                -- '(:id 1) -> (quote (:id 1))
                parseReactor "'(:id 1)"
                    `shouldBe` Right
                        (RExpr "quote" (RAtoms [RList (RProps [("id", RNumber 1)])]))

            it "parses quote of an expression" do
                -- '(set :x 1) -> (quote (set :x 1))
                parseReactor "'(set :x 1)"
                    `shouldBe` Right
                        (RExpr "quote" (RAtoms [RExpr "set" (RProps [("x", RNumber 1)])]))

            it "parses multiple quotes in different places" do
                -- (f 'a 'b)
                parseReactor "(f 'a 'b)"
                    `shouldBe` Right
                        (RExpr "f" (RAtoms [RExpr "quote" (RAtoms [RSymbol "a"]), RExpr "quote" (RAtoms [RSymbol "b"])]))

            it "parses quote of a quote of a list" do
                -- ''(1 2)
                parseReactor "''(1 2)"
                    `shouldBe` Right
                        (RExpr "quote" (RAtoms [RExpr "quote" (RAtoms [RList (RAtoms [RNumber 1, RNumber 2])])]))
