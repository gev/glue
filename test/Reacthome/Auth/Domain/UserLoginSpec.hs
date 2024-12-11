module Reacthome.Auth.Domain.UserLoginSpec (spec) where

import Data.Maybe (isJust)
import Reacthome.Auth.Domain.UserLogin (isValidUserLogin, mkUserLogin)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.QuickCheck (arbitrary, forAll, property, suchThat, (==>))
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec =
    describe "UserLogin" do
        it "creates a valid UserLogin"
            . property
            $ forAll valid \login ->
                mkUserLogin login `shouldSatisfy` isJust

        it "fails to create UserLogin with invalid input"
            . property
            $ forAll invalid \login ->
                mkUserLogin login `shouldBe` Nothing

        it "UserLogin instances with the same input are equal"
            . property
            $ forAll arbitrary \login ->
                mkUserLogin login `shouldBe` mkUserLogin login

        it "UserLogin instances with different inputs are not equal"
            . property
            $ forAll valid \login1 ->
                forAll valid \login2 ->
                    login1 /= login2 ==> mkUserLogin login1 `shouldNotBe` mkUserLogin login2
  where
    valid = arbitrary `suchThat` isValidUserLogin
    invalid = arbitrary `suchThat` (not . isValidUserLogin)
