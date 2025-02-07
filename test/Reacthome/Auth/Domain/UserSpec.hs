module Reacthome.Auth.Domain.UserSpec (spec) where

import Data.Maybe (fromJust)
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.User.Id (makeUserId)
import Reacthome.Auth.Domain.UserLogin (isValidUserLogin, makeUserLogin)
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
                makeUser uid login status
                    `shouldBeEqualTo` (uid, login, status)

        it "User instances with the same parameters are equal"
            . property
            $ forAll arbitrary \user ->
                makeUser' user `shouldBe` makeUser' user

        it "User instances with different parameters are not equal"
            . property
            $ forAll arbitrary \(user1, user2) ->
                user1 /= user2 ==> makeUser' user1 `shouldNotBe` makeUser' user2

        it "Creates a new User successfully"
            . property
            $ forAll arbitrary \(uid, login) ->
                makeNewUser uid login
                    `shouldBeEqualTo` (uid, login, Active)

        it "New User instances with the same parameters are equal"
            . property
            $ forAll arbitrary \user ->
                makeNewUser' user `shouldBe` makeNewUser' user

        it "New User instances with different parameters are not equal"
            . property
            $ forAll arbitrary \(user1, user2) ->
                user1 /= user2 ==> makeNewUser' user1 `shouldNotBe` makeNewUser' user2

        it "Change an User Login successfully"
            . property
            $ forAll arbitrary \(user, newLogin) ->
                changeUserLogin user newLogin
                    `shouldBeEqualTo` (user . uid, newLogin, user . status)

        it "Activate an User successfully"
            . property
            $ forAll arbitrary \user ->
                activateUser user
                    `shouldBeEqualTo` (user . uid, user . login, Active)

        it "Inactivate an User successfully"
            . property
            $ forAll arbitrary \user ->
                inactivateUser user
                    `shouldBeEqualTo` (user . uid, user . login, Inactive)

        it "Suspend an User successfully"
            . property
            $ forAll arbitrary \user ->
                suspendUser user
                    `shouldBeEqualTo` (user . uid, user . login, Suspended)

        it "Is User active when status is Active"
            . property
            $ forAll arbitrary \user ->
                isUserActive user
                    `shouldBe` user . status == Active

        it "Is User inactive when status is Inactive"
            . property
            $ forAll arbitrary \user ->
                isUserInactive user
                    `shouldBe` user . status == Inactive

        it "Is User suspended when status is Suspended"
            . property
            $ forAll arbitrary \user ->
                isUserSuspended user
                    `shouldBe` user . status == Suspended

type UserTuple =
    (User.Id, UserLogin, User.Status)

type NewUserTuple =
    (User.Id, UserLogin)

makeUser' :: UserTuple -> User
makeUser' (uid, login, status) = makeUser uid login status

makeNewUser' :: NewUserTuple -> User
makeNewUser' (uid, login) = makeNewUser uid login

shouldBeEqualTo ::
    User ->
    UserTuple ->
    Expectation
shouldBeEqualTo user (uid, login, status) =
    (user . uid `shouldBe` uid)
        <> (user . login `shouldBe` login)
        <> (user . status `shouldBe` status)

instance Arbitrary User where
    arbitrary = makeUser' <$> arbitrary

instance Arbitrary User.Id where
    arbitrary = makeUserId <$> arbitrary

instance Arbitrary UserLogin where
    arbitrary = makeArbitrary makeUserLogin isValidUserLogin

instance Arbitrary User.Status where
    arbitrary = elements [Active, Inactive]

makeArbitrary :: (Arbitrary a) => (a -> Maybe m) -> (a -> Bool) -> Gen m
makeArbitrary make isValid = fromJust . make <$> arbitrary `suchThat` isValid
