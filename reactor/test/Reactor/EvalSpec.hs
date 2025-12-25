module Reactor.EvalSpec (spec) where

import Data.Text (Text)
import Reactor.Error (ReactorError (..))
import Reactor.Eval as E
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..), compile)
import Reactor.Native (initialEnv)
import Reactor.Parser (parseReactor)
import Test.Hspec

runCode :: Text -> IO (Either ReactorError (Maybe E.IR))
runCode input = case parseReactor input of
    Left err -> pure $ Left (ReactorError err)
    Right ast -> do
        let irTree = compile ast
        fullResult <- runEval (eval irTree) initialEnv
        case fullResult of
            Left err -> pure $ Left (ReactorError err)
            Right (res, _finalEnv) -> pure $ Right res

spec :: Spec
spec = describe "Reactor.Eval (System Integration)" do
    it "handles basic values" do
        runCode "42" `shouldReturn` Right (Just (Number 42))
        runCode "\"test\"" `shouldReturn` Right (Just (String "test"))

    it "executes (def) and (set) chain" do
        let code = "(list (def x 1) (set x 2) x)"
        runCode code `shouldReturn` Right (Just (List [Number 2]))

    it "implements full closures (Lexical Shadowing)" do
        let code = "(((lambda (x) (lambda (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Just (Number 100))

    it "checks that (def) inside (lambda) doesn't corrupt global scope" do
        let code = "(list (def x 1) ((lambda () (def x 2))) x)"
        runCode code `shouldReturn` Right (Just (List [Number 1]))

    it "fails when calling non-existent function" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (ReactorError $ UnboundVariable "non-existent")

    it "fails when passing wrong number of arguments" do
        runCode "((lambda (a b) a) 1)"
            `shouldReturn` Left (ReactorError WrongNumberOfArguments)
