module Reactor.EvalSpec (spec) where

import Data.Text (Text)
import Reactor.Error (ReactorError (..))
import Reactor.Eval as E
import Reactor.IR (IR (..), compile)
import Reactor.Native (initialEnv)
import Reactor.Parser (parseReactor)
import Test.Hspec

-- Исправленный хелпер для запуска кода
runCode :: Text -> IO (Either ReactorError (Maybe E.IR))
runCode input = case parseReactor input of
    Left err -> pure $ Left err
    Right ast -> do
        let irTree = compile ast
        -- runEval возвращает IO (Either ReactorError (a, Env))
        fullResult <- runEval (eval irTree) initialEnv
        case fullResult of
            Left err -> pure $ Left err -- Ошибка интерпретации
            Right (res, _finalEnv) -> pure $ Right res -- Успех: берем результат, игнорируем Env

spec :: Spec
spec = describe "Reactor.Eval (Интеграция системы)" do
    it "обрабатывает простейшие значения" do
        runCode "42" `shouldReturn` Right (Just (Number 42))
        runCode "\"test\"" `shouldReturn` Right (Just (String "test"))

    it "выполняет цепочку (def) и (set)" do
        -- (def x 1) -> Nothing, (set x 2) -> Nothing, x -> 2
        -- В списке останется только финальная двойка
        let code = "(list (def x 1) (set x 2) x)"
        runCode code `shouldReturn` Right (Just (List [Number 2]))

    it "реализует полноценные замыкания (Lexical Shadowing)" do
        -- Классический тест: функция возвращает другую функцию,
        -- которая помнит локальную переменную 'x'
        let code = "(((lambda (x) (lambda (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Just (Number 100))

    it "проверяет, что (def) внутри (lambda) не портит глобальный скоуп" do
        -- Если мы хотим проверить, что x остался 1, просто вызовем x в конце
        let code = "(list (def x 1) ((lambda () (def x 2))) x)"
        runCode code `shouldReturn` Right (Just (List [Number 1]))

    it "ошибается при вызове несуществующей функции" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (SyntaxError "Unbound variable: non-existent")

    it "ошибается при передаче неверного количества аргументов" do
        runCode "((lambda (a b) a) 1)"
            `shouldReturn` Left (SyntaxError "Wrong number of arguments")
