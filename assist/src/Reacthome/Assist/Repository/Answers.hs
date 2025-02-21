module Reacthome.Assist.Repository.Answers where

import Control.Concurrent.MVar
import Control.Monad.STM
import Data.Maybe
import Reacthome.Assist.Service.Dialog
import StmContainers.Map
import Prelude hiding (lookup)

makeAnswers :: IO Answers
makeAnswers = do
    answers <- newIO

    let
        takeAnswer uid = do
            var <- newEmptyMVar
            atomically $ insert var uid answers
            takeMVar var

        putAnswer uid answer = do
            maybe
                (pure Nothing)
                (fmap Just . flip putMVar answer)
                =<< atomically (lookup uid answers)

    pure
        Answers
            { takeAnswer
            , putAnswer
            }
