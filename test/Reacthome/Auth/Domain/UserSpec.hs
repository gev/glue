module Reacthome.Auth.Domain.UserSpec (spec) where

import Data.Maybe (fromJust)
import Data.Time
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.UserId (mkUserId)
import Reacthome.Auth.Domain.UserLogin (isValidUserLogin, mkUserLogin)
import Reacthome.Auth.Domain.UserPassword (isValidUserPassword, mkUserPassword)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (arbitrary, elements, forAll, (==>))
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Gen (Gen, suchThat)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Property (property)

spec :: Spec
spec =
    describe "User" do
        it "Creates an User successfully"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                let user = mkUser uid login password time
                 in userShouldBe user uid login password Active time time

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

        it "Change an User Login successfully"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                forAll arbitrary \(newLogin, updatedAt) ->
                    let user = mkUser uid login password time
                        user' = changeUserLogin user newLogin updatedAt
                     in userShouldBe user' uid newLogin password Active time updatedAt

        it "Change an User Password successfully"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                forAll arbitrary \(newPasswordHash, updatedAt) ->
                    let user = mkUser uid login password time
                        user' = changeUserPassword user newPasswordHash updatedAt
                     in userShouldBe user' uid login newPasswordHash Active time updatedAt

        it "Change an User Status successfully"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                forAll arbitrary \(newStatus, updatedAt) ->
                    let user = mkUser uid login password time
                        user' = changeUserStatus user newStatus updatedAt
                     in userShouldBe user' uid login password newStatus time updatedAt

        it "User is active when status is Active"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                let user = mkUser uid login password time
                 in isUserActive user `shouldBe` user.status == Active

        it "User is inactive when status is Inactive"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                let user = mkUser uid login password time
                    user' = changeUserStatus user Inactive time
                 in isUserInactive user' `shouldBe` user'.status == Inactive

userShouldBe ::
    User ->
    UserId ->
    UserLogin ->
    UserPassword ->
    UserStatus ->
    UTCTime ->
    UTCTime ->
    IO ()
userShouldBe user uid login passwordHash status createdAt updatedAt =
    (user.uid `shouldBe` uid)
        <> (user.login `shouldBe` login)
        <> (user.passwordHash `shouldBe` passwordHash)
        <> (user.status `shouldBe` status)
        <> (user.createdAt `shouldBe` createdAt)
        <> (user.updatedAt `shouldBe` updatedAt)

instance Arbitrary UserId where
    arbitrary = mkUserId <$> arbitrary

instance Arbitrary UserLogin where
    arbitrary = mkArbitrary mkUserLogin isValidUserLogin

instance Arbitrary UserPassword where
    arbitrary = mkArbitrary mkUserPassword isValidUserPassword

instance Arbitrary UserStatus where
    arbitrary = elements [Active, Inactive]

mkArbitrary :: (Arbitrary a) => (a -> Maybe m) -> (a -> Bool) -> Gen m
mkArbitrary mk isValid = fromJust . mk <$> arbitrary `suchThat` isValid
