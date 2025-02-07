module Reacthome.Auth.Domain.UserIdSpec (spec) where

import Reacthome.Auth.Domain.User.Id (makeUserId)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (arbitrary, forAll, property, (==>))
import Test.QuickCheck.Instances.UUID ()

spec :: Spec
spec =
    describe "User.Id" do
        it "User.Id instances with the same UUID are equal"
            . property
            $ forAll arbitrary \uuid ->
                makeUserId uuid `shouldBe` makeUserId uuid

        it "User.Id instances with different UUIDs are not equal"
            . property
            $ forAll arbitrary \(uuid1, uuid2) ->
                uuid1 /= uuid2 ==> makeUserId uuid1 `shouldNotBe` makeUserId uuid2
