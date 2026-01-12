module Glue.Lib.IO.PrintSpec (spec) where

import Data.Text (Text)
import Glue.Error (GlueError (..))
import Glue.Eval (Eval, eval, runEvalSimple)
import Glue.IR (IR (..), compile)
import Glue.Lib.IO (io)
import Glue.Module (envFromModule)
import Glue.Parser (parseGlue)
import Test.Hspec (Spec, describe, it, shouldReturn)

runCode :: Text -> IO (Either GlueError (Maybe (IR Eval)))
runCode input = case parseGlue input of
    Left err -> pure $ Left (GlueError err)
    Right ast -> do
        let irTree = compile ast
        fullResult <- runEvalSimple (eval irTree) $ envFromModule io
        case fullResult of
            Left err -> pure $ Left (GlueError err)
            Right (res, _finalEnv, _ctx) -> pure $ Right (Just res)

spec :: Spec
spec = describe "Glue.Lib.IO.Print" do
    it "print returns void" do
        let code = "(print \"hello\")"
        runCode code `shouldReturn` Right (Just Void)

    it "println returns void" do
        let code = "(println \"hello\")"
        runCode code `shouldReturn` Right (Just Void)
