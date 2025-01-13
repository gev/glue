module Reacthome.Auth.Domain.UserSpec (spec) where

import Data.Maybe (fromJust)
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.UserId (mkUserId)
import Reacthome.Auth.Domain.UserLogin (isValidUserLogin, mkUserLogin)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, forAll, property, suchThat, (==>))
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.UUID ()

spec :: Spec
spec =
    describe "User" do
        it "Creates an User successfully"
            . property
            $ forAll arbitrary \(uid, login, status) ->
                mkUser uid login status
                    `shouldBeEqualTo` (uid, login, status)

        it "User instances with the same parameters are equal"
            . property
            $ forAll arbitrary \user ->
                mkUser' user `shouldBe` mkUser' user

        it "User instances with different parameters are not equal"
            . property
            $ forAll arbitrary \(user1, user2) ->
                user1 /= user2 ==> mkUser' user1 `shouldNotBe` mkUser' user2

        it "Creates a new User successfully"
            . property
            $ forAll arbitrary \(uid, login) ->
                mkNewUser uid login
                    `shouldBeEqualTo` (uid, login, Active)

        it "New User instances with the same parameters are equal"
            . property
            $ forAll arbitrary \user ->
                mkNewUser' user `shouldBe` mkNewUser' user

        it "New User instances with different parameters are not equal"
            . property
            $ forAll arbitrary \(user1, user2) ->
                user1 /= user2 ==> mkNewUser' user1 `shouldNotBe` mkNewUser' user2

        it "Change an User Login successfully"
            . property
            $ forAll arbitrary \(user, newLogin) ->
                changeUserLogin user newLogin
                    `shouldBeEqualTo` (user.uid, newLogin, user.status)

        it "Activate an User successfully"
            . property
            $ forAll arbitrary \user ->
                activateUser user
                    `shouldBeEqualTo` (user.uid, user.login, Active)

        it "Inactivate an User successfully"
            . property
            $ forAll arbitrary \user ->
                inactivateUser user
                    `shouldBeEqualTo` (user.uid, user.login, Inactive)

        it "Suspend an User successfully"
            . property
            $ forAll arbitrary \user ->
                suspendUser user
                    `shouldBeEqualTo` (user.uid, user.login, Suspended)

        it "Is User active when status is Active"
            . property
            $ forAll arbitrary \user ->
                isUserActive user
                    `shouldBe` user.status == Active

        it "Is User inactive when status is Inactive"
            . property
            $ forAll arbitrary \user ->
                isUserInactive user
                    `shouldBe` user.status == Inactive

        it "Is User suspended when status is Suspended"
            . property
            $ forAll arbitrary \user ->
                isUserSuspended user
                    `shouldBe` user.status == Suspended

type UserTuple =
    (UserId, UserLogin, UserStatus)

type NewUserTuple =
    (UserId, UserLogin)

mkUser' :: UserTuple -> User
mkUser' (uid, login, status) = mkUser uid login status

mkNewUser' :: NewUserTuple -> User
mkNewUser' (uid, login) = mkNewUser uid login

shouldBeEqualTo ::
    User ->
    UserTuple ->
    Expectation
shouldBeEqualTo user (uid, login, status) =
    (user.uid `shouldBe` uid)
        <> (user.login `shouldBe` login)
        <> (user.status `shouldBe` status)

instance Arbitrary User where
    arbitrary = mkUser' <$> arbitrary

instance Arbitrary UserId where
    arbitrary = mkUserId <$> arbitrary

instance Arbitrary UserLogin where
    arbitrary = mkArbitrary mkUserLogin isValidUserLogin

instance Arbitrary UserStatus where
    arbitrary = elements [Active, Inactive]

mkArbitrary :: (Arbitrary a) => (a -> Maybe m) -> (a -> Bool) -> Gen m
mkArbitrary mk isValid = fromJust . mk <$> arbitrary `suchThat` isValid
