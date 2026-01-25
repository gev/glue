module Glue.Lib.Bool.WhileSpec (spec) where

import Glue.Env qualified as E
import Glue.Eval (apply, runEvalSimple)
import Glue.IR (IR (..))
import Glue.Lib.Bool.While (while_)
import Glue.Lib.Builtin (builtin)
import Glue.Module (envFromModule)
import Glue.Runtime (Runtime (..))
import Test.Hspec

spec :: Spec
spec = describe "Glue.Lib.Bool.While (Test while special form)" do
    describe "Loop while condition" do
        it "returns nothing when condition is false and no body" do
            let args = [Bool False] -- No body, should return nothing
            result <- runEvalSimple (apply while_ args) []
            case result of
                Left err -> expectationFailure $ "While failed: " <> show err
                Right (res, _) -> res `shouldBe` Void
