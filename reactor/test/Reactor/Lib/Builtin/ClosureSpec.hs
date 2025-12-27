module Reactor.Lib.Builtin.ClosureSpec (spec) where

import Data.Either (isLeft)
import Reactor.Env
import Reactor.IR
import Reactor.Lib.Builtin.Lambda (extractSymbols, makeClosure)
import Test.Hspec
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "Reactor.Lib.Builtin.Closure (Test closures)" do
    it "extractSymbols: correctly extracts list of names" do
        let input = [Symbol "a", Symbol "b"]
        extractSymbols input `shouldBe` Right ["a", "b"]

    it "extractSymbols: fails if list contains non-symbols" do
        let input = [Symbol "a", Number 1]
        extractSymbols input `shouldSatisfy` isLeft

    it "makeClosure: packs parameters and body, preserving Env" do
        let env = fromList [("x", Number 10)]
        let closure = makeClosure ["a"] (Symbol "x") env
        case closure of
            Closure ["a"] (Symbol "x") savedEnv ->
                lookupVar "x" savedEnv `shouldBe` Right (Number 10)
            _ -> expectationFailure "Invalid closure structure"
