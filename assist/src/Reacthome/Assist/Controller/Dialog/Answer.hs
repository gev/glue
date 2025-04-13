module Reacthome.Assist.Controller.Dialog.Answer where

import Data.Aeson
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import Reacthome.Assist.Dialog.Gate
import Reacthome.Assist.Service.Dialog

handleAnswer ::
    (?answers :: Answers) =>
    Text ->
    IO ()
handleAnswer message =
    either
        (const $ pure ()) -- print
        (uncurry setAnswer)
        (unpack =<< eitherDecode (encodeUtf8 message))
