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
                case parseReactor "123" of
                    Right (Number n) -> n `shouldBe` 123
                    _ -> expectationFailure "Should parse as integer"

            it "parses negative numbers" $ do
                case parseReactor "-42" of
                    Right (Number n) -> n `shouldBe` (-42)
                    _ -> expectationFailure "Should parse as negative number"

            it "parses floats" $ do
                case parseReactor "3.14" of
                    Right (Number n) -> n `shouldBe` 3.14
                    _ -> expectationFailure "Should parse as float"

            it "parses negative floats" $ do
                case parseReactor "-2.71" of
                    Right (Number n) -> n `shouldSatisfy` (\x -> abs (x - (-2.71)) < 0.001)
                    _ -> expectationFailure "Should parse as negative float"

            it "parses scientific notation" $ do
                case parseReactor "1.23e4" of
                    Right (Number n) -> n `shouldBe` 12300
                    _ -> expectationFailure "Should parse scientific notation"
                case parseReactor "1.23E4" of
                    Right (Number n) -> n `shouldBe` 12300
                    _ -> expectationFailure "Should parse uppercase E"
                case parseReactor "1.23e-2" of
                    Right (Number n) -> n `shouldSatisfy` (\x -> abs (x - 0.0123) < 0.0001)
                    _ -> expectationFailure "Should parse negative exponent"
                case parseReactor "-1.23e4" of
                    Right (Number n) -> n `shouldBe` (-12300)
                    _ -> expectationFailure "Should parse negative scientific"

            it "parses strings" $ do
                case parseReactor "\"hello\"" of
                    Right (String s) -> s `shouldBe` "hello"
                    _ -> expectationFailure "Should parse as string"

            it "parses symbols" $ do
                case parseReactor "void" of
                    Right (Symbol s) -> s `shouldBe` "void"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "my-func" of
                    Right (Symbol s) -> s `shouldBe` "my-func"
                    _ -> expectationFailure "Should parse as symbol"

            it "parses symbols starting with letter" $ do
                case parseReactor "a" of
                    Right (Symbol s) -> s `shouldBe` "a"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "z" of
                    Right (Symbol s) -> s `shouldBe` "z"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "A" of
                    Right (Symbol s) -> s `shouldBe` "A"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "Z" of
                    Right (Symbol s) -> s `shouldBe` "Z"
                    _ -> expectationFailure "Should parse as symbol"

            it "parses symbols with dots" $ do
                case parseReactor "math.utils" of
                    Right (Symbol s) -> s `shouldBe` "math.utils"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "list.utils" of
                    Right (Symbol s) -> s `shouldBe` "list.utils"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "my.module.name" of
                    Right (Symbol s) -> s `shouldBe` "my.module.name"
                    _ -> expectationFailure "Should parse as symbol"

        describe "Operator Symbols" $ do
            it "parses arithmetic operators" $ do
                case parseReactor "+" of
                    Right (Symbol s) -> s `shouldBe` "+"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "*" of
                    Right (Symbol s) -> s `shouldBe` "*"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "/" of
                    Right (Symbol s) -> s `shouldBe` "/"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "%" of
                    Right (Symbol s) -> s `shouldBe` "%"
                    _ -> expectationFailure "Should parse as symbol"

            it "parses comparison operators" $ do
                case parseReactor "<" of
                    Right (Symbol s) -> s `shouldBe` "<"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor ">" of
                    Right (Symbol s) -> s `shouldBe` ">"
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "<=" of
                    Right (Symbol s) -> s `shouldBe` "<="
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor ">=" of
                    Right (Symbol s) -> s `shouldBe` ">="
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "==" of
                    Right (Symbol s) -> s `shouldBe` "=="
                    _ -> expectationFailure "Should parse as symbol"
                case parseReactor "\\=" of
                    Right (Symbol s) -> s `shouldBe` "\\="
                    _ -> expectationFailure "Should parse as symbol"

            it "parses logical operators" $ do
                case parseReactor "!" of
                    Right (Symbol s) -> s `shouldBe` "!"
                    _ -> expectationFailure "Should parse as symbol"

            it "parses complex operator expressions" $ do
                case parseReactor "(+ 2 3)" of
                    Right (AtomList [Symbol "+", Number 2, Number 3]) -> pure ()
                    _ -> expectationFailure "Should parse as operator expression"
                case parseReactor "(< x y)" of
                    Right (AtomList [Symbol "<", Symbol "x", Symbol "y"]) -> pure ()
                    _ -> expectationFailure "Should parse as comparison expression"
                case parseReactor "(== a b)" of
                    Right (AtomList [Symbol "==", Symbol "a", Symbol "b"]) -> pure ()
                    _ -> expectationFailure "Should parse as equality expression"

        describe "Rule: No Mixed Content" $ do
            it "successfully parses pure positional list" $ do
                let input = "(1 2 \"test\")"
                case parseReactor input of
                    Right (AtomList [Number 1, Number 2, String "test"]) -> pure ()
                    _ -> expectationFailure "Should be AtomList"

            it "successfully parses pure property list" $ do
                let input = "(:id 1 :type \"lamp\")"
                case parseReactor input of
                    Right (PropList [("id", Number 1), ("type", String "lamp")]) -> pure ()
                    _ -> expectationFailure "Should be PropList"

            it "FAILS when mixing atoms and properties" $ do
                case parseReactor "(:id 1 \"oops\")" of
                    Left (MixedContent "\"oops\"") -> pure ()
                    _ -> expectationFailure "Should fail with MixedContent"

            it "FAILS when mixing properties and atoms" $ do
                case parseReactor "(1 2 :id 3)" of
                    Left (MixedContent ":id") -> pure ()
                    _ -> expectationFailure "Should fail with MixedContent"

        describe "Rule: Property Pairs" $ do
            it "FAILS on unpaired property key" $ do
                case parseReactor "(:id 1 :status)" of
                    Left (UnpairedProperty ":status") -> pure ()
                    _ -> expectationFailure "Should fail with UnpairedProperty"

        describe "Syntax Errors" $ do
            it "wraps Megaparsec errors into SyntaxError" $ do
                case parseReactor "(unclosed list" of
                    Left (SyntaxError _) -> pure ()
                    _ -> expectationFailure "Expected a SyntaxError for unclosed parenthesis"

        describe "Quote sugar" $ do
            it "parses quoted symbols as (quote symbol)" $ do
                case parseReactor "'foo" of
                    Right (AtomList [Symbol "quote", Symbol "foo"]) -> pure ()
                    _ -> expectationFailure "Should parse as quoted symbol"

            it "parses quoted lists" $ do
                case parseReactor "'(1 2)" of
                    Right (AtomList [Symbol "quote", AtomList [Number 1, Number 2]]) -> pure ()
                    _ -> expectationFailure "Should parse as quoted list"

        describe "Advanced Quote sugar" do
            it "parses nested quotes (quote of quote)" do
                -- ''foo -> (quote (quote foo))
                case parseReactor "''foo" of
                    Right (AtomList [Symbol "quote", AtomList [Symbol "quote", Symbol "foo"]]) -> pure ()
                    _ -> expectationFailure "Should parse nested quotes"

            it "parses quote inside a list" do
                -- ('a 1) -> ((quote a) 1)
                case parseReactor "('a 1)" of
                    Right (AtomList [AtomList [Symbol "quote", Symbol "a"], Number 1]) -> pure ()
                    _ -> expectationFailure "Should parse quote inside list"

            it "parses quote of a list with properties" do
                -- '(:id 1) -> (quote (:id 1))
                case parseReactor "'(:id 1)" of
                    Right (AtomList [Symbol "quote", PropList [("id", Number 1)]]) -> pure ()
                    _ -> expectationFailure "Should parse quoted property list"

            it "parses quote of an expression" do
                -- '(set :x 1) -> (quote (set :x 1))
                case parseReactor "'(set :x 1)" of
                    Right (AtomList [Symbol "quote", AtomList [Symbol "set", PropList [("x", Number 1)]]]) -> pure ()
                    _ -> expectationFailure "Should parse quoted expression"

            it "parses multiple quotes in different places" do
                -- (f 'a 'b)
                case parseReactor "(f 'a 'b)" of
                    Right (AtomList [Symbol "f", AtomList [Symbol "quote", Symbol "a"], AtomList [Symbol "quote", Symbol "b"]]) -> pure ()
                    _ -> expectationFailure "Should parse multiple quotes"

            it "parses quote of a quote of a list" do
                -- ''(1 2)
                case parseReactor "''(1 2)" of
                    Right (AtomList [Symbol "quote", AtomList [Symbol "quote", AtomList [Number 1, Number 2]]]) -> pure ()
                    _ -> expectationFailure "Should parse quote of quote of list"

        describe "Property Access" $ do
            it "parses property access" $ do
                case parseReactor "obj.name" of
                    Right (Symbol "obj.name") -> pure ()
                    _ -> expectationFailure "Should parse as property access"

            it "parses property access with complex object" $ do
                case parseReactor "obj.name" of
                    Right (Symbol "obj.name") -> pure ()
                    _ -> expectationFailure "Should parse as property access"

            it "parses nested property access" $ do
                case parseReactor "a.b.c" of
                    Right (Symbol "a.b.c") -> pure ()
                    _ -> expectationFailure "Should parse as nested property access"

            it "parses property access on quoted expression" $ do
                case parseReactor "'foo.bar" of
                    Right (AtomList [Symbol "quote", Symbol "foo.bar"]) -> pure ()
                    _ -> expectationFailure "Should parse quoted property access"

            it "parses property access with numbers in name" $ do
                case parseReactor "obj.prop1" of
                    Right (Symbol "obj.prop1") -> pure ()
                    _ -> expectationFailure "Should parse property access with numbers"

            it "parses property access with special chars in name" $ do
                case parseReactor "obj.prop-name" of
                    Right (Symbol "obj.prop-name") -> pure ()
                    _ -> expectationFailure "Should parse property access with special chars"
                case parseReactor "obj.prop_name" of
                    Right (Symbol "obj.prop_name") -> pure ()
                    _ -> expectationFailure "Should parse property access with underscores"

        describe "Equivalent Syntaxes" $ do
            it "parses (f :x 1) and (f (:x 1)) identically" $ do
                let expected = Right (AtomList [Symbol "f", PropList [("x", Number 1)]])
                parseReactor "(f :x 1)" `shouldBe` expected
                parseReactor "(f (:x 1))" `shouldBe` expected

            it "parses (f :x 1 :y 2) and (f (:x 1) (:y 2)) differently" $ do
                let grouped = parseReactor "(f :x 1 :y 2)"
                let separate = parseReactor "(f (:x 1) (:y 2))"
                grouped `shouldNotBe` separate
                case grouped of
                    Right (AtomList [Symbol "f", PropList [("x", Number 1), ("y", Number 2)]]) -> pure ()
                    _ -> expectationFailure "Should parse grouped properties"
                case separate of
                    Right (AtomList [Symbol "f", PropList [("x", Number 1)], PropList [("y", Number 2)]]) -> pure ()
                    _ -> expectationFailure "Should parse separate properties"

            it "parses lambda with named arg" $ do
                let input = "((lambda (it) it) (:it 1))"
                case parseReactor input of
                    Right (AtomList [AtomList [Symbol "lambda", AtomList [Symbol "it"], Symbol "it"], PropList [("it", Number 1)]]) -> pure ()
                    _ -> expectationFailure "Should parse lambda with named arg"

            it "parses lambda with multiple named args" $ do
                let input = "((lambda (it) it) (:it 1) (:yet 2))"
                case parseReactor input of
                    Right (AtomList [AtomList [Symbol "lambda", AtomList [Symbol "it"], Symbol "it"], PropList [("it", Number 1)], PropList [("yet", Number 2)]]) -> pure ()
                    _ -> expectationFailure "Should parse lambda with multiple named args"
