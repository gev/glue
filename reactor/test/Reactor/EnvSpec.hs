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
import Reactor.Value

-- Настройка типов для тестов
type V = Value Identity
type E = Env Identity

-- Генератор для простейших значений Value
instance Arbitrary (Value Identity) where
    arbitrary =
        oneof
            [ VNumber <$> arbitrary
            , VSymbol <$> arbitrary
            , VString <$> arbitrary
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
            let env = fromList [("a", VNumber 1), ("b", VNumber 2)] :: E
            lookupLocal "a" env `shouldBe` Just (VNumber 1)
            lookupVar "b" env `shouldBe` Right (VNumber 2)

        it "pushFrame / popFrame: управляют глубиной стека (LIFO)" do
            let base = fromList [("x", VNumber 1)]
            let pushed = pushFrame base
            -- В новом слое переменной "x" локально нет
            lookupLocal "x" pushed `shouldBe` Nothing
            -- Но через поиск вглубь она видна
            lookupVar "x" pushed `shouldBe` Right (VNumber 1)
            -- После popFrame возвращаемся к исходному состоянию
            popFrame pushed `shouldBe` base

        it "popFrame на пустом списке не падает" do
            popFrame ([] :: E) `shouldBe` []

    describe "Запись и поиск (Shadowing)" do
        prop "defineVar: всегда пишет в текущий (верхний) слой" $ \name (val :: V) -> do
            let env = pushFrame (fromList [("other", VNumber 0)])
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
            let vOld = VNumber 10
            let vNew = VNumber 20
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
            makeQuote [VNumber 42] `shouldBe` Right (VNumber 42)

        it "makeQuote: возвращает ошибку, если аргументов нет или их много" do
            makeQuote [] `shouldSatisfy` isLeft
            makeQuote [VNumber 1, VNumber 2] `shouldSatisfy` isLeft

        it "extractSymbols: корректно извлекает список имен" do
            let input = [VSymbol "a", VSymbol "b"]
            extractSymbols input `shouldBe` Right ["a", "b"]

        it "extractSymbols: падает, если в списке есть не-символ" do
            let input = [VSymbol "a", VNumber 1]
            extractSymbols input `shouldSatisfy` isLeft

        it "makeClosure: упаковывает параметры и тело, сохраняя Env" do
            let env = fromList [("x", VNumber 10)]
            let closure = makeClosure ["a"] (VSymbol "x") env
            case closure of
                VClosure ["a"] (VSymbol "x") savedEnv ->
                    lookupVar "x" savedEnv `shouldBe` Right (VNumber 10)
                _ -> expectationFailure "Неверная структура замыкания"
