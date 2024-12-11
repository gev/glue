module Reacthome.Auth.Domain.UserIdSpec (spec) where

import Reacthome.Auth.Domain.UserId (mkUserId)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.QuickCheck (arbitrary, forAll, property, (==>))
import Test.QuickCheck.Instances.UUID ()

spec :: Spec
spec =
    describe "UserId" do
        it "UserId instances with the same UUID are equal"
            . property
            $ forAll arbitrary \uuid ->
                mkUserId uuid `shouldBe` mkUserId uuid

        it "UserId instances with different UUIDs are not equal"
            . property
            $ forAll arbitrary \(uuid1, uuid2) ->
                uuid1 /= uuid2 ==> mkUserId uuid1 `shouldNotBe` mkUserId uuid2
