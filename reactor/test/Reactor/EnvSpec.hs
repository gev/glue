{-# OPTIONS_GHC -Wno-orphans #-}

module Reactor.EnvSpec (spec) where

import Data.Either (isLeft)
import Data.Functor.Identity (Identity)
import Reactor.Env
import Reactor.IR
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

type V = IR Identity
type E = Env Identity
instance Arbitrary (IR Identity) where
    arbitrary =
        oneof
            [ Number <$> arbitrary
            , Symbol <$> arbitrary
            , String <$> arbitrary
            ]

spec :: Spec
spec = describe "Reactor.Env (Test stack memory model)" do
    describe "Smart constructors and atomic operations" do
        it "emptyEnv: create empty stack environment" do
            let env = emptyEnv :: E
            lookupLocal "any" env `shouldBe` Nothing
            lookupVar "any" env `shouldSatisfy` \case
                Left _ -> True
                _ -> False

        it "fromList: initialize environment stack" do
            let env = fromList [("a", Number 1), ("b", Number 2)] :: E
            lookupLocal "a" env `shouldBe` Just (Number 1)
            lookupVar "b" env `shouldBe` Right (Number 2)

        it "pushFrame / popFrame: manage stack (LIFO)" do
            let base = fromList [("x", Number 1)]
            let pushed = pushFrame base
            lookupLocal "x" pushed `shouldBe` Nothing
            lookupVar "x" pushed `shouldBe` Right (Number 1)
            popFrame pushed `shouldBe` base

        it "popFrame don't crash on empty stack" do
            popFrame ([] :: E) `shouldBe` []

    describe "Define and search (shadowing)" do
        prop "defineVar: always define at the top frame" $ \name (val :: V) -> do
            let env = pushFrame (fromList [("other", Number 0)])
            let newEnv = defineVar name val env
            lookupLocal name newEnv `shouldBe` Just val

        prop
            "Shadowing: local definition shadow global"
            \name (vLocal :: V) (vGlobal :: V) -> do
                let env = fromList [(name, vGlobal)]
                let finalEnv = defineVar name vLocal (pushFrame env)
                lookupVar name finalEnv `shouldBe` Right vLocal
                lookupVar name (popFrame finalEnv) `shouldBe` Right vGlobal

    describe "Variable updating" do
        it "updateVar: update values in the place, don't create a new one" do
            let vOld = Number 10
            let vNew = Number 20
            let env = pushFrame (fromList [("x", vOld)])

            case updateVar "x" vNew env of
                Left err -> expectationFailure $ "Update failed: " <> show err
                Right updatedEnv -> do
                    lookupLocal "x" updatedEnv `shouldBe` Nothing
                    lookupVar "x" updatedEnv `shouldBe` Right vNew

        prop
            "updateVar: return error for unbound variable"
            \name (v :: V) -> do
                let env = emptyEnv :: E
                updateVar name v env `shouldSatisfy` isLeft

    describe "Safety lookup" do
        it "lookupLocal: returns Nothing on the empty stack" do
            lookupLocal "x" [] `shouldBe` Nothing

        prop
            "lookupVar: returns error on empty stack "
            \name -> do
                lookupVar name [] `shouldSatisfy` isLeft
