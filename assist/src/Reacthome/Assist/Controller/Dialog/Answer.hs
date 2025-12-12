module Reacthome.Assist.Controller.Dialog.Answer where

import Data.Aeson (eitherDecode)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Reacthome.Assist.Dialog.Gate (setAnswer)
import Reacthome.Assist.Service.Dialog (Answers, unpack)

handleAnswer ::
    (?answers :: Answers) =>
    Text ->
    IO ()
handleAnswer message =
    either
        (const $ pure ()) -- print
        (uncurry setAnswer)
        (unpack =<< eitherDecode (encodeUtf8 message))
