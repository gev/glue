{-# OPTIONS_GHC -Wno-orphans #-}

module Reactor.ValueSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Functor.Identity (Identity)
import Reactor.AST
import Reactor.Value

-- 1. Генератор для самого Reactor (AST)
instance Arbitrary Reactor where
    arbitrary = sized genReactor
      where
        genReactor n
            | n <= 0 =
                oneof
                    [ RSymbol <$> arbitrary
                    , RNumber <$> arbitrary
                    , RString <$> arbitrary
                    ]
        genReactor n =
            oneof
                [ RSymbol <$> arbitrary
                , RNumber <$> arbitrary
                , RString <$> arbitrary
                , -- Генерируем вложенные структуры
                  RList . RAtoms <$> resize (n `div` 2) arbitrary
                , RList . RProps <$> resize (n `div` 2) arbitrary
                , RExpr <$> arbitrary <*> (RAtoms <$> resize (n `div` 2) arbitrary)
                , RExpr <$> arbitrary <*> (RProps <$> resize (n `div` 2) arbitrary)
                ]

spec :: Spec
spec = describe "Трансформация AST -> Value (prepare)" do
    -- Позиционные списки
    prop "атомы: длина списка сохраняется 1 к 1" $ \(xs :: [Reactor]) -> do
        let ast = RList (RAtoms xs)
        let val = prepare ast :: Value Identity
        case val of
            VList ys -> length ys `shouldBe` length xs
            _ -> expectationFailure "Ожидался VList"

    -- Списки свойств (ключ-значение)
    prop "свойства: количество элементов удваивается (ключ + значение)" $ \(ps :: [(T.Text, Reactor)]) -> do
        let ast = RList (RProps ps)
        let val = prepare ast :: Value Identity
        case val of
            VList ys -> length ys `shouldBe` (length ps * 2)
            _ -> expectationFailure "Ожидался VList"

    -- Проверка префикса свойств
    prop "ключи в Value всегда начинаются с ':'" $ \(ps :: [(T.Text, Reactor)]) ->
        not (null ps) ==> do
            let ast = RList (RProps ps)
            let val = prepare ast :: Value Identity
            case val of
                VList (VSymbol k : _) -> T.isPrefixOf ":" k `shouldBe` True
                _ -> expectationFailure "Первый элемент должен быть символом-ключом"

    -- Вызовы функций (RExpr)
    prop "имя вызова VCall совпадает с именем RExpr" $ \(name :: T.Text) (xs :: [Reactor]) -> do
        let ast = RExpr name (RAtoms xs)
        let val = prepare ast :: Value Identity
        case val of
            VCall n _ -> n `shouldBe` name
            _ -> expectationFailure "Ожидался VCall"

    -- Рекурсивная целостность (не падает ли на глубоких деревьях)
    prop "целостность: любая структура AST успешно трансформируется" $ \(ast :: Reactor) -> do
        let val = prepare ast :: Value Identity
        -- Если prepare не упал с исключением, тест пройден
        seq val True `shouldBe` True

    -- Кейс из визуального редактора (свойства в вызове)
    prop "вызов со свойствами разворачивается корректно" $ \(name :: T.Text) (ps :: [(T.Text, Reactor)]) -> do
        let ast = RExpr name (RProps ps)
        let val = prepare ast :: Value Identity
        case val of
            VCall _ args -> length args `shouldBe` (length ps * 2)
            _ -> expectationFailure "Ожидался VCall"

    prop "пустые атомы и пропсы не создают мусорных данных" $ \name -> do
        let ast1 = RList (RAtoms [])
        let ast2 = RExpr name (RProps [])
        (prepare ast1 :: Value Identity) `shouldBe` VList []
        (prepare ast2 :: Value Identity) `shouldBe` VCall name []

    prop "RSymbol переходит в VSymbol без изменений (идемпотентность)" $ \s -> do
        let ast = RSymbol s
        let val = prepare ast :: Value Identity
        case val of
            VSymbol s' -> s' `shouldBe` s
            _ -> expectationFailure "Должен быть VSymbol"

    prop "рекурсивная развертка: свойства могут содержать вложенные вызовы" $ \k (astInner :: Reactor) -> do
        let ast = RList (RProps [(k, astInner)])
        let val = prepare ast :: Value Identity
        case val of
            VList [VSymbol k', valInner] -> do
                k' `shouldBe` (":" <> k)
                valInner `shouldBe` (prepare astInner :: Value Identity)
            _ -> expectationFailure "Ошибка рекурсивной развертки"
