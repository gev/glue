module Reactor.ParserSpec (spec) where

import Reactor.AST
import Reactor.Parser (parseReactor)
import Reactor.Parser.Error (ParserError (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "Reactor LISP Parser" $ do
        describe "Basic Atoms" $ do
            it "parses integers" $ do
                parseReactor "123" `shouldBe` Right (Number 123)

            it "parses strings" $ do
                parseReactor "\"hello\"" `shouldBe` Right (String "hello")

            it "parses symbols" $ do
                parseReactor "void" `shouldBe` Right (Symbol "void")
                parseReactor "my-func" `shouldBe` Right (Symbol "my-func")

        describe "Rule: No Mixed Content" $ do
            it "successfully parses pure positional list" $ do
                let input = "(1 2 \"test\")"
                case parseReactor input of
                    Right (List (Atoms [Number 1, Number 2, String "test"])) -> pure ()
                    _ -> expectationFailure "Should be Atoms"

            it "successfully parses pure property list" $ do
                let input = "(:id 1 :type \"lamp\")"
                case parseReactor input of
                    Right (List (Props [("id", Number 1), ("type", String "lamp")])) -> pure ()
                    _ -> expectationFailure "Should be Props"

            it "FAILS when mixing atoms and properties" $ do
                parseReactor "(:id 1 \"oops\")" `shouldBe` Left (MixedContent "\"oops\"")

            it "FAILS when mixing properties and atoms" $ do
                parseReactor "(1 2 :id 3)" `shouldBe` Left (MixedContent ":id")

        describe "Rule: Property Pairs" $ do
            it "FAILS on unpaired property key" $ do
                parseReactor "(:id 1 :status)" `shouldBe` Left (UnpairedProperty ":status")

        describe "Syntax Errors" $ do
            it "wraps Megaparsec errors into SyntaxError" $ do
                case parseReactor "(unclosed list" of
                    Left (SyntaxError _) -> pure ()
                    _ -> expectationFailure "Expected a SyntaxError for unclosed parenthesis"

        describe "Quote sugar" $ do
            it "parses quoted symbols as (quote symbol)" $ do
                parseReactor "'foo" `shouldBe` Right (List (Atoms [Symbol "quote", Symbol "foo"]))

            it "parses quoted lists" $ do
                parseReactor "'(1 2)" `shouldBe` Right (List (Atoms [Symbol "quote", List (Atoms [Number 1, Number 2])]))

        describe "Advanced Quote sugar" do
            it "parses nested quotes (quote of quote)" do
                -- ''foo -> (quote (quote foo))
                parseReactor "''foo"
                    `shouldBe` Right
                        (List (Atoms [Symbol "quote", List (Atoms [Symbol "quote", Symbol "foo"])]))

            it "parses quote inside a list" do
                -- (list 'a 1) -> (list (quote a) 1)
                parseReactor "(list 'a 1)"
                    `shouldBe` Right
                        (List (Atoms [Symbol "list", List (Atoms [Symbol "quote", Symbol "a"]), Number 1]))

            it "parses quote of a list with properties" do
                -- '(:id 1) -> (quote (:id 1))
                parseReactor "'(:id 1)"
                    `shouldBe` Right
                        (List (Atoms [Symbol "quote", List (Props [("id", Number 1)])]))

            it "parses quote of an expression" do
                -- '(set :x 1) -> (quote (set :x 1))
                parseReactor "'(set :x 1)"
                    `shouldBe` Right
                        (List (Atoms [Symbol "quote", List (Atoms [Symbol "set", List (Props [("x", Number 1)])])]))

            it "parses multiple quotes in different places" do
                -- (f 'a 'b)
                parseReactor "(f 'a 'b)"
                    `shouldBe` Right
                        (List (Atoms [Symbol "f", List (Atoms [Symbol "quote", Symbol "a"]), List (Atoms [Symbol "quote", Symbol "b"])]))

            it "parses quote of a quote of a list" do
                -- ''(1 2)
                parseReactor "''(1 2)"
                    `shouldBe` Right
                        (List (Atoms [Symbol "quote", List (Atoms [Symbol "quote", List (Atoms [Number 1, Number 2])])]))
