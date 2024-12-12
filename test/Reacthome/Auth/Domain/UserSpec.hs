module Reacthome.Auth.Domain.UserSpec (spec) where

import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Reacthome.Auth.Domain.User
import Reacthome.Auth.Domain.UserId (mkUserId)
import Reacthome.Auth.Domain.UserLogin (isValidUserLogin, mkUserLogin)
import Reacthome.Auth.Domain.UserPassword (isValidUserPassword, mkUserPassword)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, forAll, property, suchThat, (==>))
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.UUID ()

spec :: Spec
spec =
    describe "User" do
        it "Creates an User successfully"
            . property
            $ forAll arbitrary \(uid, login, password, time) ->
                let user = mkUser uid login password time
                 in user `shouldBeEqualTo` (uid, login, password, Active, time, time)

        it "User instances with the same parameters are equal"
            . property
            $ forAll arbitrary \user ->
                let user1 = mkUser' user
                    user2 = mkUser' user
                 in user1 `shouldBe` user2

        it "User instances with different parameters are not equal"
            . property
            $ forAll arbitrary \(user1, user2) ->
                user1 /= user2 ==> mkUser' user1 `shouldNotBe` mkUser' user2

        it "Change an User Login successfully"
            . property
            $ forAll arbitrary \(uid, login, password, createdAt) ->
                forAll arbitrary \(newLogin, updatedAt) ->
                    let user = mkUser uid login password createdAt
                        user' = changeUserLogin user newLogin updatedAt
                     in user' `shouldBeEqualTo` (uid, newLogin, password, Active, createdAt, updatedAt)

        it "Change an User Password successfully"
            . property
            $ forAll arbitrary \(uid, login, password, createdAt) ->
                forAll arbitrary \(newPassword, updatedAt) ->
                    let user = mkUser uid login password createdAt
                        user' = changeUserPassword user newPassword updatedAt
                     in user' `shouldBeEqualTo` (uid, login, newPassword, Active, createdAt, updatedAt)

        it "Change an User Status successfully"
            . property
            $ forAll arbitrary \(uid, login, password, createdAt) ->
                forAll arbitrary \(newStatus, updatedAt) ->
                    let user = mkUser uid login password createdAt
                        user' = changeUserStatus user newStatus updatedAt
                     in user' `shouldBeEqualTo` (uid, login, password, newStatus, createdAt, updatedAt)

        it "User is active when status is Active"
            . property
            $ forAll arbitrary \user ->
                forAll arbitrary \(status, time) ->
                    let user' = changeUserStatus user status time
                     in isUserActive user' `shouldBe` status == Active

        it "User is inactive when status is Inactive"
            . property
            $ forAll arbitrary \user ->
                forAll arbitrary \(status, time) ->
                    let user' = changeUserStatus user status time
                     in isUserInactive user' `shouldBe` status == Inactive

        it "User is suspended when status is Suspended"
            . property
            $ forAll arbitrary \user ->
                forAll arbitrary \(status, time) ->
                    let user' = changeUserStatus user status time
                     in isUserSuspended user' `shouldBe` status == Suspended

type UserParams =
    (UserId, UserLogin, UserPassword, UTCTime)

type UserTuple =
    (UserId, UserLogin, UserPassword, UserStatus, UTCTime, UTCTime)

mkUser' :: UserParams -> User
mkUser' (uid, login, passwordHash, time) = mkUser uid login passwordHash time

shouldBeEqualTo ::
    User ->
    UserTuple ->
    Expectation
shouldBeEqualTo user (uid, login, passwordHash, status, createdAt, updatedAt) =
    (user.uid `shouldBe` uid)
        <> (user.login `shouldBe` login)
        <> (user.passwordHash `shouldBe` passwordHash)
        <> (user.status `shouldBe` status)
        <> (user.createdAt `shouldBe` createdAt)
        <> (user.updatedAt `shouldBe` updatedAt)

instance Arbitrary User where
    arbitrary = mkUser' <$> arbitrary

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
