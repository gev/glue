module Reacthome.Auth.Domain.UserSpec (spec) where

import Data.Maybe (fromJust)
import Reacthome.Auth.Domain.User (UserId, UserLogin, UserPassword, UserStatus (..), createdAt, login, mkActiveUser, mkUser, passwordHash, status, uid, updatedAt)
import Reacthome.Auth.Domain.UserId (mkUserId)
import Reacthome.Auth.Domain.UserLogin (isValidUserLogin, mkUserLogin)
import Reacthome.Auth.Domain.UserPassword (isValidUserPassword, mkUserPassword)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (arbitrary, forAll, (==>))
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Gen (Gen, suchThat)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Property (property)

spec :: Spec
spec =
    describe "User" do
        it "Creates a User successfully"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                let user = mkUser uid login password time
                 in (user.uid `shouldBe` uid)
                        <> (user.login `shouldBe` login)
                        <> (user.passwordHash `shouldBe` password)
                        <> (user.status `shouldBe` Inactive)
                        <> (user.createdAt `shouldBe` time)
                        <> (user.updatedAt `shouldBe` time)

        it "Creates an active User successfully"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                let user = mkActiveUser uid login password time
                 in (user.uid `shouldBe` uid)
                        <> (user.login `shouldBe` login)
                        <> (user.passwordHash `shouldBe` password)
                        <> (user.status `shouldBe` Active)
                        <> (user.createdAt `shouldBe` time)
                        <> (user.updatedAt `shouldBe` time)

        it "User instances with the same parameters are equal"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                let user1 = mkUser uid login password time
                    user2 = mkUser uid login password time
                 in user1 `shouldBe` user2

        it "User instances with different parameters are not equal"
            . property
            $ forAll arbitrary \(uid1, login1, password1, time1) ->
                forAll arbitrary \(uid2, login2, password2, time2) ->
                    (uid1, login1, password1, time1)
                        /= (uid2, login2, password2, time2)
                            ==> mkUser uid1 login1 password1 time1
                        `shouldNotBe` mkUser uid2 login2 password2 time2

instance Arbitrary UserId where
    arbitrary = mkUserId <$> arbitrary

instance Arbitrary UserLogin where
    arbitrary = mkArbitrary mkUserLogin isValidUserLogin

instance Arbitrary UserPassword where
    arbitrary = mkArbitrary mkUserPassword isValidUserPassword

mkArbitrary :: (Arbitrary a) => (a -> Maybe m) -> (a -> Bool) -> Gen m
mkArbitrary mk isValid = fromJust . mk <$> arbitrary `suchThat` isValid
