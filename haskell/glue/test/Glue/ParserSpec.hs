module Glue.ParserSpec (spec) where

import Data.Either (isLeft)
import Glue.AST
import Glue.Parser (parseGlue)
import Glue.Parser.Error (ParserError (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "Glue LISP Parser" $ do
        describe "Atoms" $ do
            describe "Numbers" $ do
                it "parses integers" $ do
                    parseGlue "123" `shouldBe` Right (Integer 123)

                it "parses negative numbers" $ do
                    parseGlue "-42" `shouldBe` Right (Integer (-42))

                it "parses floats" $ do
                    parseGlue "3.14" `shouldBe` Right (Float 3.14)

                it "parses scientific notation" $ do
                    parseGlue "1.23e4" `shouldBe` Right (Float 12300)
                    parseGlue "1.23E4" `shouldBe` Right (Float 12300)
                    parseGlue "1.23e+4" `shouldBe` Right (Float 12300)
                    parseGlue "1.23E+4" `shouldBe` Right (Float 12300)
                    parseGlue "1.23e-4" `shouldBe` Right (Float 0.000123)
                    parseGlue "1.23E-4" `shouldBe` Right (Float 0.000123)
                    parseGlue "-1.23e4" `shouldBe` Right (Float (-12300))
                    parseGlue "-1.23E4" `shouldBe` Right (Float (-12300))
                    parseGlue "-1.23e+4" `shouldBe` Right (Float (-12300))
                    parseGlue "-1.23E+4" `shouldBe` Right (Float (-12300))
                    parseGlue "-1.23e-4" `shouldBe` Right (Float (-0.000123))
                    parseGlue "-1.23E-4" `shouldBe` Right (Float (-0.000123))

            describe "Strings" $ do
                it "parses strings" $ do
                    parseGlue "\"hello\"" `shouldBe` Right (String "hello")

            describe "Symbols" $ do
                it "parses basic symbols" $ do
                    parseGlue "void" `shouldBe` Right (Symbol "void")
                    parseGlue "my-func" `shouldBe` Right (Symbol "my-func")

                it "parses symbols starting with letters" $ do
                    parseGlue "a" `shouldBe` Right (Symbol "a")
                    parseGlue "z" `shouldBe` Right (Symbol "z")
                    parseGlue "A" `shouldBe` Right (Symbol "A")
                    parseGlue "Z" `shouldBe` Right (Symbol "Z")

                describe "Special Characters" $ do
                    describe "Arithmetic operators" $ do
                        it "parses arithmetic symbols" $ do
                            parseGlue "+" `shouldBe` Right (Symbol "+")
                            parseGlue "-" `shouldBe` Right (Symbol "-")
                            parseGlue "*" `shouldBe` Right (Symbol "*")
                            parseGlue "/" `shouldBe` Right (Symbol "/")
                            parseGlue "%" `shouldBe` Right (Symbol "%")

                    describe "Comparison operators" $ do
                        it "parses comparison symbols" $ do
                            parseGlue "=" `shouldBe` Right (Symbol "=")
                            parseGlue "<" `shouldBe` Right (Symbol "<")
                            parseGlue ">" `shouldBe` Right (Symbol ">")

                    describe "Logical operators" $ do
                        it "parses logical symbols" $ do
                            parseGlue "&" `shouldBe` Right (Symbol "&")
                            parseGlue "|" `shouldBe` Right (Symbol "|")
                            parseGlue "!" `shouldBe` Right (Symbol "!")

                    describe "Separators and punctuation" $ do
                        it "parses separator symbols" $ do
                            parseGlue "?" `shouldBe` Right (Symbol "?")
                            parseGlue "\\" `shouldBe` Right (Symbol "\\")
                            parseGlue "$" `shouldBe` Right (Symbol "$")
                            parseGlue "@" `shouldBe` Right (Symbol "@")
                            parseGlue "#" `shouldBe` Right (Symbol "#")
                            parseGlue "_" `shouldBe` Right (Symbol "_")
                            parseGlue "." `shouldBe` Right (Symbol ".")

                it "parses symbols with dots" $ do
                    parseGlue "math.utils" `shouldBe` Right (Symbol "math.utils")
                    parseGlue "list.utils" `shouldBe` Right (Symbol "list.utils")
                    parseGlue "my.module.name" `shouldBe` Right (Symbol "my.module.name")

                it "parses symbols with colon in continuation" $ do
                    parseGlue "obj:key" `shouldBe` Right (Symbol "obj:key")
                    parseGlue "ns:module:item" `shouldBe` Right (Symbol "ns:module:item")

                it "parses complex symbol combinations" $ do
                    parseGlue "func$helper" `shouldBe` Right (Symbol "func$helper")
                    parseGlue "data@2023" `shouldBe` Right (Symbol "data@2023")
                    parseGlue "item#1" `shouldBe` Right (Symbol "item#1")
                    parseGlue "path/to:item" `shouldBe` Right (Symbol "path/to:item")

                it "parses symbols starting with digits (fallback for invalid numbers)" $ do
                    parseGlue "123abc" `shouldSatisfy` isLeft
                    parseGlue "42invalid" `shouldSatisfy` isLeft

        describe "Operator Expressions" $ do
            it "parses complex operator expressions" $ do
                parseGlue "(+ 2 3)" `shouldBe` Right (List [Symbol "+", Integer 2, Integer 3])
                parseGlue "(< x y)" `shouldBe` Right (List [Symbol "<", Symbol "x", Symbol "y"])
                parseGlue "(== a b)" `shouldBe` Right (List [Symbol "==", Symbol "a", Symbol "b"])
                parseGlue "(* 2 3 4)" `shouldBe` Right (List [Symbol "*", Integer 2, Integer 3, Integer 4])
                parseGlue "(<= x 10)" `shouldBe` Right (List [Symbol "<=", Symbol "x", Integer 10])

        describe "Rule: No Mixed Content" $ do
            it "successfully parses pure list" $ do
                let input = "(1 2 \"test\")"
                parseGlue input `shouldBe` Right (List [Integer 1, Integer 2, String "test"])

            it "successfully parses pure object" $ do
                let input = "(:id 1 :type \"lamp\")"
                parseGlue input `shouldBe` Right (Object [("id", Integer 1), ("type", String "lamp")])

            it "FAILS when mixing atoms and properties" $ do
                parseGlue "(:id 1 \"oops\")" `shouldBe` Left (MixedContent "\"oops\"")

            it "FAILS when mixing properties and atoms" $ do
                parseGlue "(1 2 :id 3)" `shouldBe` Left (MixedContent ":id")

        describe "Rule: Property Pairs" $ do
            it "FAILS on unpaired property key" $ do
                parseGlue "(:id 1 :status)" `shouldBe` Left (UnpairedProperty ":status")

        describe "Syntax Errors" $ do
            it "wraps Megaparsec errors into SyntaxError" $ do
                parseGlue "(unclosed list" `shouldSatisfy` isLeft

        describe "Property Access" $ do
            it "parses property access" $ do
                parseGlue "obj.name" `shouldBe` Right (Symbol "obj.name")

            it "parses property access with complex object" $ do
                parseGlue "obj.name" `shouldBe` Right (Symbol "obj.name")

            it "parses nested property access" $ do
                parseGlue "a.b.c" `shouldBe` Right (Symbol "a.b.c")

            it "parses property access with numbers in name" $ do
                parseGlue "obj.prop1" `shouldBe` Right (Symbol "obj.prop1")

            it "parses property access with special chars in name" $ do
                parseGlue "obj.prop-name" `shouldBe` Right (Symbol "obj.prop-name")
                parseGlue "obj.prop_name" `shouldBe` Right (Symbol "obj.prop_name")

        describe "Equivalent Syntaxes" $ do
            it "parses (f :x 1) and (f (:x 1)) identically" $ do
                let expected = Right (List [Symbol "f", Object [("x", Integer 1)]])
                parseGlue "(f :x 1)" `shouldBe` expected
                parseGlue "(f (:x 1))" `shouldBe` expected

            it "parses (f :x 1 :y 2) and (f (:x 1) (:y 2)) differently" $ do
                let grouped = parseGlue "(f :x 1 :y 2)"
                let separate = parseGlue "(f (:x 1) (:y 2))"
                grouped `shouldNotBe` separate
                grouped `shouldBe` Right (List [Symbol "f", Object [("x", Integer 1), ("y", Integer 2)]])
                separate `shouldBe` Right (List [Symbol "f", Object [("x", Integer 1)], Object [("y", Integer 2)]])

            it "parses lambda with named arg" $ do
                let input = "((lambda (it) it) (:it 1))"
                let expected = Right (List [List [Symbol "lambda", List [Symbol "it"], Symbol "it"], Object [("it", Integer 1)]])
                parseGlue input `shouldBe` expected

            it "parses lambda with multiple named args" $ do
                let input = "((lambda (it) it) (:it 1) (:yet 2))"
                let expected = Right (List [List [Symbol "lambda", List [Symbol "it"], Symbol "it"], Object [("it", Integer 1)], Object [("yet", Integer 2)]])
                parseGlue input `shouldBe` expected

        describe "Edge Cases" $ do
            it "parses empty input" $ do
                parseGlue "" `shouldSatisfy` isLeft

            it "parses whitespace only" $ do
                parseGlue "   \n\t  " `shouldSatisfy` isLeft

            it "parses deeply nested structures" $ do
                let input = "((((a))))"
                parseGlue input `shouldBe` Right (List [List [List [List [Symbol "a"]]]])

            it "parses mixed nested objects and lists" $ do
                let input = "(:a (:b (1 2)) :c 3)"
                let expected = Right (Object [("a", Object [("b", List [Integer 1, Integer 2])]), ("c", Integer 3)])
                parseGlue input `shouldBe` expected

        describe "Error Recovery" $ do
            it "provides meaningful error messages" $ do
                let result = parseGlue "(+ 1"
                result `shouldSatisfy` isLeft
            -- The error should contain information about the parsing failure

            it "handles malformed numbers" $ do
                parseGlue "123.456.789" `shouldSatisfy` isLeft
                parseGlue "12.34e56e78" `shouldSatisfy` isLeft

        describe "Unicode and Special Characters" $ do
            it "handles unicode in strings" $ do
                parseGlue "\"hello 世界\"" `shouldBe` Right (String "hello 世界")

            it "handles escape sequences in strings" $ do
                parseGlue "\"hello\\nworld\"" `shouldBe` Right (String "hello\\nworld")

            it "handles symbols with unicode" $ do
                parseGlue "变量" `shouldBe` Right (Symbol "变量")
