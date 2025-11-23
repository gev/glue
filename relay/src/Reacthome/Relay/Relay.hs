module Reacthome.Relay.Relay where

import Control.Concurrent.STM (STM, readTBQueue, writeTChan)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan)
import Control.Exception (catch, throw)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (LazyRaw, Uid, getMessageDestination)
import StmContainers.Map (insert, lookup, newIO)
import Prelude hiding (lookup, show)

data Relay = Relay
    { sendMessage :: LazyRaw -> STM ()
    , getSource :: Uid -> STM Source
    , dispatch :: IO ()
    }

type Source = TChan LazyRaw

makeRelay :: Int -> IO Relay
makeRelay bound = do
    sink <- newTBQueueIO $ fromIntegral bound
    sources <- newIO

    let
        sendMessage = writeTBQueue sink

        getSource uid =
            lookup uid sources
                >>= maybe
                    do
                        source <- newBroadcastTChan
                        insert source uid sources
                        dupTChan source
                    dupTChan

        dispatch = forever do
            catch @RelayError
                do atomically run
                logError

        run = do
            message <- readTBQueue sink
            let destination = getMessageDestination message
            source <-
                lookup destination sources
                    >>= maybe
                        do throw $ NoPeersFound destination
                        do pure
            writeTChan source message

    pure Relay{..}
