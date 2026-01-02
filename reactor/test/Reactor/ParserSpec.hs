module Reactor.ParserSpec (spec) where

import Data.Either (isLeft)
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

            it "parses negative numbers" $ do
                parseReactor "-42" `shouldBe` Right (Number (-42))

            it "parses floats" $ do
                parseReactor "3.14" `shouldBe` Right (Number 3.14)

            -- it "parses negative floats" $ do
            --     parseReactor "-2.71" `shouldBe` Right (Number (-2.71))

            it "parses scientific notation" $ do
                parseReactor "1.23e4" `shouldBe` Right (Number 12300)
                parseReactor "1.23E4" `shouldBe` Right (Number 12300)
                parseReactor "1.23e+4" `shouldBe` Right (Number 12300)
                parseReactor "1.23E+4" `shouldBe` Right (Number 12300)
                parseReactor "1.23e-4" `shouldBe` Right (Number 0.000123)
                parseReactor "1.23E-4" `shouldBe` Right (Number 0.000123)

                parseReactor "-1.23e4" `shouldBe` Right (Number (-12300))
                parseReactor "-1.23E4" `shouldBe` Right (Number (-12300))
                parseReactor "-1.23e+4" `shouldBe` Right (Number (-12300))
                parseReactor "-1.23E+4" `shouldBe` Right (Number (-12300))
                parseReactor "-1.23e-4" `shouldBe` Right (Number (-0.000123))
                parseReactor "-1.23E-4" `shouldBe` Right (Number (-0.000123))

            it "parses strings" $ do
                parseReactor "\"hello\"" `shouldBe` Right (String "hello")

            it "parses symbols" $ do
                parseReactor "void" `shouldBe` Right (Symbol "void")
                parseReactor "my-func" `shouldBe` Right (Symbol "my-func")

            it "parses symbols starting with letter" $ do
                parseReactor "a" `shouldBe` Right (Symbol "a")
                parseReactor "z" `shouldBe` Right (Symbol "z")
                parseReactor "A" `shouldBe` Right (Symbol "A")
                parseReactor "Z" `shouldBe` Right (Symbol "Z")

            it "parses symbols with dots" $ do
                parseReactor "math.utils" `shouldBe` Right (Symbol "math.utils")
                parseReactor "list.utils" `shouldBe` Right (Symbol "list.utils")
                parseReactor "my.module.name" `shouldBe` Right (Symbol "my.module.name")

            it "parses symbols with new special characters" $ do
                parseReactor "$var" `shouldBe` Right (Symbol "$var")
                parseReactor "@user" `shouldBe` Right (Symbol "@user")
                parseReactor "#id" `shouldBe` Right (Symbol "#id")
                parseReactor "&ref" `shouldBe` Right (Symbol "&ref")
                parseReactor "|pipe" `shouldBe` Right (Symbol "|pipe")
                parseReactor "'quote" `shouldBe` Right (Symbol "'quote")

            it "parses symbols with colon in continuation" $ do
                parseReactor "obj:key" `shouldBe` Right (Symbol "obj:key")
                parseReactor "ns:module:item" `shouldBe` Right (Symbol "ns:module:item")

            it "parses complex symbol combinations" $ do
                parseReactor "func$helper" `shouldBe` Right (Symbol "func$helper")
                parseReactor "data@2023" `shouldBe` Right (Symbol "data@2023")
                parseReactor "item#1" `shouldBe` Right (Symbol "item#1")
                parseReactor "path/to:item" `shouldBe` Right (Symbol "path/to:item")

        describe "Operator Symbols" $ do
            it "parses arithmetic operators" $ do
                parseReactor "+" `shouldBe` Right (Symbol "+")
                parseReactor "*" `shouldBe` Right (Symbol "*")
                parseReactor "/" `shouldBe` Right (Symbol "/")
                parseReactor "%" `shouldBe` Right (Symbol "%")

            it "parses comparison operators" $ do
                parseReactor "<" `shouldBe` Right (Symbol "<")
                parseReactor ">" `shouldBe` Right (Symbol ">")
                parseReactor "<=" `shouldBe` Right (Symbol "<=")
                parseReactor ">=" `shouldBe` Right (Symbol ">=")
                parseReactor "==" `shouldBe` Right (Symbol "==")
                parseReactor "\\=" `shouldBe` Right (Symbol "\\=")

            it "parses logical operators" $ do
                parseReactor "!" `shouldBe` Right (Symbol "!")

            it "parses complex operator expressions" $ do
                parseReactor "(+ 2 3)" `shouldBe` Right (AtomList [Symbol "+", Number 2, Number 3])
                parseReactor "(< x y)" `shouldBe` Right (AtomList [Symbol "<", Symbol "x", Symbol "y"])
                parseReactor "(== a b)" `shouldBe` Right (AtomList [Symbol "==", Symbol "a", Symbol "b"])

        describe "Rule: No Mixed Content" $ do
            it "successfully parses pure positional list" $ do
                let input = "(1 2 \"test\")"
                parseReactor input `shouldBe` Right (AtomList [Number 1, Number 2, String "test"])

            it "successfully parses pure property list" $ do
                let input = "(:id 1 :type \"lamp\")"
                parseReactor input `shouldBe` Right (PropList [("id", Number 1), ("type", String "lamp")])

            it "FAILS when mixing atoms and properties" $ do
                parseReactor "(:id 1 \"oops\")" `shouldBe` Left (MixedContent "\"oops\"")

            it "FAILS when mixing properties and atoms" $ do
                parseReactor "(1 2 :id 3)" `shouldBe` Left (MixedContent ":id")

        describe "Rule: Property Pairs" $ do
            it "FAILS on unpaired property key" $ do
                parseReactor "(:id 1 :status)" `shouldBe` Left (UnpairedProperty ":status")

        describe "Syntax Errors" $ do
            it "wraps Megaparsec errors into SyntaxError" $ do
                parseReactor "(unclosed list" `shouldSatisfy` isLeft

        describe "Quote sugar" $ do
            it "parses quoted symbols as (quote symbol)" $ do
                parseReactor "'foo" `shouldBe` Right (AtomList [Symbol "quote", Symbol "foo"])

            it "parses quoted lists" $ do
                parseReactor "'(1 2)" `shouldBe` Right (AtomList [Symbol "quote", AtomList [Number 1, Number 2]])

        describe "Advanced Quote sugar" do
            it "parses nested quotes (quote of quote)" do
                -- ''foo -> (quote (quote foo))
                parseReactor "''foo"
                    `shouldBe` Right (AtomList [Symbol "quote", AtomList [Symbol "quote", Symbol "foo"]])

            it "parses quote inside a list" do
                -- ('a 1) -> ((quote a) 1)
                parseReactor "('a 1)"
                    `shouldBe` Right (AtomList [AtomList [Symbol "quote", Symbol "a"], Number 1])

            it "parses quote of a list with properties" do
                -- '(:id 1) -> (quote (:id 1))
                parseReactor "'(:id 1)"
                    `shouldBe` Right (AtomList [Symbol "quote", PropList [("id", Number 1)]])

            it "parses quote of an expression" do
                -- '(set :x 1) -> (quote (set :x 1))
                parseReactor "'(set :x 1)"
                    `shouldBe` Right (AtomList [Symbol "quote", AtomList [Symbol "set", PropList [("x", Number 1)]]])

            it "parses multiple quotes in different places" do
                -- (f 'a 'b)
                parseReactor "(f 'a 'b)"
                    `shouldBe` Right (AtomList [Symbol "f", AtomList [Symbol "quote", Symbol "a"], AtomList [Symbol "quote", Symbol "b"]])

            it "parses quote of a quote of a list" do
                -- ''(1 2)
                parseReactor "''(1 2)"
                    `shouldBe` Right
                        (AtomList [Symbol "quote", AtomList [Symbol "quote", AtomList [Number 1, Number 2]]])

        describe "Property Access" $ do
            it "parses property access" $ do
                parseReactor "obj.name" `shouldBe` Right (Symbol "obj.name")

            it "parses property access with complex object" $ do
                parseReactor "obj.name" `shouldBe` Right (Symbol "obj.name")

            it "parses nested property access" $ do
                parseReactor "a.b.c" `shouldBe` Right (Symbol "a.b.c")

            it "parses property access on quoted expression" $ do
                parseReactor "'foo.bar" `shouldBe` Right (AtomList [Symbol "quote", Symbol "foo.bar"])

            it "parses property access with numbers in name" $ do
                parseReactor "obj.prop1" `shouldBe` Right (Symbol "obj.prop1")

            it "parses property access with special chars in name" $ do
                parseReactor "obj.prop-name" `shouldBe` Right (Symbol "obj.prop-name")
                parseReactor "obj.prop_name" `shouldBe` Right (Symbol "obj.prop_name")

        describe "Equivalent Syntaxes" $ do
            it "parses (f :x 1) and (f (:x 1)) identically" $ do
                let expected = Right (AtomList [Symbol "f", PropList [("x", Number 1)]])
                parseReactor "(f :x 1)" `shouldBe` expected
                parseReactor "(f (:x 1))" `shouldBe` expected

            it "parses (f :x 1 :y 2) and (f (:x 1) (:y 2)) differently" $ do
                let grouped = parseReactor "(f :x 1 :y 2)"
                let separate = parseReactor "(f (:x 1) (:y 2))"
                grouped `shouldNotBe` separate
                grouped `shouldBe` Right (AtomList [Symbol "f", PropList [("x", Number 1), ("y", Number 2)]])
                separate `shouldBe` Right (AtomList [Symbol "f", PropList [("x", Number 1)], PropList [("y", Number 2)]])

            it "parses lambda with named arg" $ do
                let input = "((lambda (it) it) (:it 1))"
                let expected = Right (AtomList [AtomList [Symbol "lambda", AtomList [Symbol "it"], Symbol "it"], PropList [("it", Number 1)]])
                parseReactor input `shouldBe` expected

            it "parses lambda with multiple named args" $ do
                let input = "((lambda (it) it) (:it 1) (:yet 2))"
                let expected = Right (AtomList [AtomList [Symbol "lambda", AtomList [Symbol "it"], Symbol "it"], PropList [("it", Number 1)], PropList [("yet", Number 2)]])
                parseReactor input `shouldBe` expected
