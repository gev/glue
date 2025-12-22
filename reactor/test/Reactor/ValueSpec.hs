{-# OPTIONS_GHC -Wno-orphans #-}

module Reactor.ValueSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Functor.Identity (Identity)
import Reactor.AST (AST)
import Reactor.AST qualified as AST
import Reactor.IR (IR, compile)
import Reactor.IR qualified as IR

-- 1. Генератор для самого Reactor (AST)
instance Arbitrary AST where
    arbitrary = sized genReactor
      where
        genReactor n
            | n <= 0 =
                oneof
                    [ AST.Symbol <$> arbitrary
                    , AST.Number <$> arbitrary
                    , AST.String <$> arbitrary
                    ]
        genReactor n =
            oneof
                [ AST.Symbol <$> arbitrary
                , AST.Number <$> arbitrary
                , AST.String <$> arbitrary
                , -- Генерируем вложенные структуры
                  AST.List . AST.Atoms <$> resize (n `div` 2) arbitrary
                , AST.List . AST.Props <$> resize (n `div` 2) arbitrary
                , AST.Expr <$> arbitrary <*> (AST.Atoms <$> resize (n `div` 2) arbitrary)
                , AST.Expr <$> arbitrary <*> (AST.Props <$> resize (n `div` 2) arbitrary)
                ]

spec :: Spec
spec = describe "Трансформация AST -> IR (compile)" do
    -- Позиционные списки
    prop "атомы: длина списка сохраняется 1 к 1" $ \(xs :: [AST]) -> do
        let ast = AST.List (AST.Atoms xs)
        let val = compile ast :: IR Identity
        case val of
            IR.List ys -> length ys `shouldBe` length xs
            _ -> expectationFailure "Ожидался List"

    -- Списки свойств (ключ-значение)
    prop "свойства: количество элементов удваивается (ключ + значение)" $ \(ps :: [(T.Text, AST)]) -> do
        let ast = AST.List (AST.Props ps)
        let val = compile ast :: IR Identity
        case val of
            IR.List ys -> length ys `shouldBe` (length ps * 2)
            _ -> expectationFailure "Ожидался List"

    -- Проверка префикса свойств
    prop "ключи в IR всегда начинаются с ':'" $ \(ps :: [(T.Text, AST)]) ->
        not (null ps) ==> do
            let ast = AST.List (AST.Props ps)
            let val = compile ast :: IR Identity
            case val of
                IR.List (IR.Symbol k : _) -> T.isPrefixOf ":" k `shouldBe` True
                _ -> expectationFailure "Первый элемент должен быть символом-ключом"

    -- Вызовы функций (Expr)
    prop "имя вызова Call совпадает с именем Expr" $ \(name :: T.Text) (xs :: [AST]) -> do
        let ast = AST.Expr name (AST.Atoms xs)
        let val = compile ast :: IR Identity
        case val of
            IR.Call n _ -> n `shouldBe` name
            _ -> expectationFailure "Ожидался Call"

    -- Рекурсивная целостность (не падает ли на глубоких деревьях)
    prop "целостность: любая структура AST успешно трансформируется" $ \(ast :: AST) -> do
        let val = compile ast :: IR Identity
        -- Если compile не упал с исключением, тест пройден
        seq val True `shouldBe` True

    -- Кейс из визуального редактора (свойства в вызове)
    prop "вызов со свойствами разворачивается корректно" $ \(name :: T.Text) (ps :: [(T.Text, AST)]) -> do
        let ast = AST.Expr name (AST.Props ps)
        let val = compile ast :: IR Identity
        case val of
            IR.Call _ args -> length args `shouldBe` (length ps * 2)
            _ -> expectationFailure "Ожидался Call"

    prop "пустые атомы и пропсы не создают мусорных данных" $ \name -> do
        let ast1 = AST.List (AST.Atoms [])
        let ast2 = AST.Expr name (AST.Props [])
        (compile ast1 :: IR Identity) `shouldBe` IR.List []
        (compile ast2 :: IR Identity) `shouldBe` IR.Call name []

    prop "RSymbol переходит в Symbol без изменений (идемпотентность)" $ \s -> do
        let ast = AST.Symbol s
        let val = compile ast :: IR Identity
        case val of
            IR.Symbol s' -> s' `shouldBe` s
            _ -> expectationFailure "Должен быть Symbol"

    prop "рекурсивная развертка: свойства могут содержать вложенные вызовы" $ \k (astInner :: AST) -> do
        let ast = AST.List (AST.Props [(k, astInner)])
        let val = compile ast :: IR Identity
        case val of
            IR.List [IR.Symbol k', valInner] -> do
                k' `shouldBe` (":" <> k)
                valInner `shouldBe` (compile astInner :: IR Identity)
            _ -> expectationFailure "Ошибка рекурсивной развертки"
