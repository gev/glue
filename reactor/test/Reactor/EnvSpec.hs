{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reactor.EnvSpec (spec) where

import Data.Functor.Identity (Identity)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Either (isLeft)
import Reactor.Env
import Reactor.Error (ReactorError (..))
import Reactor.IR

-- Настройка типов для тестов
type V = IR Identity
type E = Env Identity

-- Генератор для простейших значений IR
instance Arbitrary (IR Identity) where
    arbitrary =
        oneof
            [ Number <$> arbitrary
            , Symbol <$> arbitrary
            , String <$> arbitrary
            ]

spec :: Spec
spec = describe "Reactor.Env (Тестирование системы памяти)" do
    describe "Смарт-конструкторы и атомарные операции" do
        it "emptyEnv: создает рабочее окружение с одним пустым слоем" do
            let env = emptyEnv :: E
            lookupLocal "any" env `shouldBe` Nothing
            lookupVar "any" env `shouldSatisfy` \case
                Left (SyntaxError _) -> True
                _ -> False

        it "fromList: корректно инициализирует начальный фрейм" do
            let env = fromList [("a", Number 1), ("b", Number 2)] :: E
            lookupLocal "a" env `shouldBe` Just (Number 1)
            lookupVar "b" env `shouldBe` Right (Number 2)

        it "pushFrame / popFrame: управляют глубиной стека (LIFO)" do
            let base = fromList [("x", Number 1)]
            let pushed = pushFrame base
            -- В новом слое переменной "x" локально нет
            lookupLocal "x" pushed `shouldBe` Nothing
            -- Но через поиск вглубь она видна
            lookupVar "x" pushed `shouldBe` Right (Number 1)
            -- После popFrame возвращаемся к исходному состоянию
            popFrame pushed `shouldBe` base

        it "popFrame на пустом списке не падает" do
            popFrame ([] :: E) `shouldBe` []

    describe "Запись и поиск (Shadowing)" do
        prop "defineVar: всегда пишет в текущий (верхний) слой" $ \name (val :: V) -> do
            let env = pushFrame (fromList [("other", Number 0)])
            let newEnv = defineVar name val env
            lookupLocal name newEnv `shouldBe` Just val

        prop "Shadowing: локальное определение скрывает глобальное" $
            \name (vLocal :: V) (vGlobal :: V) -> do
                let env = fromList [(name, vGlobal)]
                let finalEnv = defineVar name vLocal (pushFrame env)
                -- Должны видеть локальное
                lookupVar name finalEnv `shouldBe` Right vLocal
                -- После удаления локального слоя — снова глобальное
                lookupVar name (popFrame finalEnv) `shouldBe` Right vGlobal

    describe "Обновление переменных (updateVar)" do
        it "updateVar: меняет значение там, где оно найдено, не создавая новых записей" do
            let vOld = Number 10
            let vNew = Number 20
            -- Стек: [локальный (пустой), глобальный (x=10)]
            let env = pushFrame (fromList [("x", vOld)])

            case updateVar "x" vNew env of
                Left err -> expectationFailure $ "Update failed: " ++ show err
                Right updatedEnv -> do
                    -- 1. Убеждаемся, что в локальном слое по-прежнему ПУСТО
                    lookupLocal "x" updatedEnv `shouldBe` Nothing
                    -- 2. Убеждаемся, что значение во всем стеке обновилось
                    lookupVar "x" updatedEnv `shouldBe` Right vNew

        prop "updateVar: возвращает ошибку, если переменная не была определена ранее" $
            \name (v :: V) -> do
                let env = emptyEnv :: E
                updateVar name v env `shouldSatisfy` \case
                    Left (SyntaxError _) -> True
                    _ -> False

    describe "Безопасность и Lookup" do
        it "lookupLocal на пустом стеке возвращает Nothing" do
            lookupLocal "x" [] `shouldBe` Nothing

        prop "lookupVar на пустом стеке возвращает SyntaxError" $ \name -> do
            lookupVar name [] `shouldSatisfy` \case
                Left (SyntaxError _) -> True
                _ -> False

    describe "Специальные формы и валидация" do
        it "makeQuote: успешно извлекает единственный аргумент" do
            makeQuote [Number 42] `shouldBe` Right (Number 42)

        it "makeQuote: возвращает ошибку, если аргументов нет или их много" do
            makeQuote [] `shouldSatisfy` isLeft
            makeQuote [Number 1, Number 2] `shouldSatisfy` isLeft

        it "extractSymbols: корректно извлекает список имен" do
            let input = [Symbol "a", Symbol "b"]
            extractSymbols input `shouldBe` Right ["a", "b"]

        it "extractSymbols: падает, если в списке есть не-символ" do
            let input = [Symbol "a", Number 1]
            extractSymbols input `shouldSatisfy` isLeft

        it "makeClosure: упаковывает параметры и тело, сохраняя Env" do
            let env = fromList [("x", Number 10)]
            let closure = makeClosure ["a"] (Symbol "x") env
            case closure of
                Closure ["a"] (Symbol "x") savedEnv ->
                    lookupVar "x" savedEnv `shouldBe` Right (Number 10)
                _ -> expectationFailure "Неверная структура замыкания"
