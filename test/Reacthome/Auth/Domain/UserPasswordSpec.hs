module Reacthome.Auth.Domain.UserPasswordSpec (spec) where

import Data.Maybe (isJust)
import Reacthome.Auth.Domain.UserPassword (isValidUserPassword, mkUserPassword)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.QuickCheck (arbitrary, forAll, property, suchThat, (==>))
import Test.QuickCheck.Instances.Text ()

spec :: Spec
spec =
    describe "UserPassword" do
        it "Creates a valid UserPassword"
            . property
            $ forAll valid \password ->
                mkUserPassword password `shouldSatisfy` isJust

        it "Fails to create UserPassword with invalid input"
            . property
            $ forAll invalid \password ->
                mkUserPassword password `shouldBe` Nothing

        it "UserPassword instances with the same input are equal"
            . property
            $ forAll arbitrary \password ->
                mkUserPassword password `shouldBe` mkUserPassword password

        it "UserPassword instances with different inputs are not equal"
            . property
            $ forAll valid \password1 ->
                forAll valid \password2 ->
                    password1 /= password2 ==> mkUserPassword password1 `shouldNotBe` mkUserPassword password2
  where
    valid = arbitrary `suchThat` isValidUserPassword
    invalid = arbitrary `suchThat` (not . isValidUserPassword)
