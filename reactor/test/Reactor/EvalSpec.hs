module Reactor.EvalSpec (spec) where

import Data.Text (Text)
import Reactor.Error (ReactorError (..))
import Reactor.Eval as E
import Reactor.Native (initialEnv)
import Reactor.Parser (parseReactor)
import Reactor.Value (Value (..), prepare)
import Test.Hspec

-- Исправленный хелпер для запуска кода
runCode :: Text -> IO (Either ReactorError (Maybe E.Value))
runCode input = case parseReactor input of
    Left err -> pure $ Left err
    Right ast -> do
        let valTree = prepare ast
        -- runEval возвращает IO (Either ReactorError (a, Env))
        fullResult <- runEval (eval valTree) initialEnv
        case fullResult of
            Left err -> pure $ Left err -- Ошибка интерпретации
            Right (res, _finalEnv) -> pure $ Right res -- Успех: берем результат, игнорируем Env

spec :: Spec
spec = describe "Reactor.Eval (Интеграция системы)" do
    it "обрабатывает простейшие значения" do
        runCode "42" `shouldReturn` Right (Just (VNumber 42))
        runCode "\"test\"" `shouldReturn` Right (Just (VString "test"))

    it "выполняет цепочку (def) и (set)" do
        -- (def x 1) -> Nothing, (set x 2) -> Nothing, x -> 2
        -- В списке останется только финальная двойка
        let code = "(list (def x 1) (set x 2) x)"
        runCode code `shouldReturn` Right (Just (VList [VNumber 2]))

    it "реализует полноценные замыкания (Lexical Shadowing)" do
        -- Классический тест: функция возвращает другую функцию,
        -- которая помнит локальную переменную 'x'
        let code = "(((lambda (x) (lambda (y) x)) 100) 1)"
        runCode code `shouldReturn` Right (Just (VNumber 100))

    it "проверяет, что (def) внутри (lambda) не портит глобальный скоуп" do
        -- Если мы хотим проверить, что x остался 1, просто вызовем x в конце
        let code = "(list (def x 1) ((lambda () (def x 2))) x)"
        runCode code `shouldReturn` Right (Just (VList [VNumber 1]))

    it "ошибается при вызове несуществующей функции" do
        runCode "(non-existent 1 2)"
            `shouldReturn` Left (SyntaxError "Unbound variable: non-existent")

    it "ошибается при передаче неверного количества аргументов" do
        runCode "((lambda (a b) a) 1)"
            `shouldReturn` Left (SyntaxError "Wrong number of arguments")
