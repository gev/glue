module Reacthome.Relay.Relay where

import Control.Concurrent.STM (readTBQueue, writeTChan)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan)
import Control.Exception (catch, throw)
import Control.Monad (forever, void)
import Control.Monad.STM (atomically)
import Data.HashMap.Strict (empty, insert, lookup)
import GHC.IORef (atomicModifyIORef'_, newIORef, readIORef)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (LazyRaw, Uid, getMessageDestination)
import Prelude hiding (lookup, show)

data Relay = Relay
    { sendMessage :: LazyRaw -> IO ()
    , getSource :: Uid -> IO Source
    , dispatch :: IO ()
    }

type Source = TChan LazyRaw

makeRelay :: Int -> IO Relay
makeRelay bound = do
    sink <- newTBQueueIO $ fromIntegral bound
    sources <- newIORef empty

    let
        sendMessage = atomically . writeTBQueue sink

        getSource uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> do
                    (source, clone) <- atomically do
                        source <- newBroadcastTChan
                        clone <- dupTChan source
                        pure (source, clone)
                    void $ atomicModifyIORef'_ sources $ insert uid source
                    pure clone
                Just source -> atomically $ dupTChan source

        dispatch = forever do
            sources' <- readIORef sources
            catch @RelayError
                do
                    message <- atomically $ readTBQueue sink
                    let destination = getMessageDestination message
                    case lookup destination sources' of
                        Nothing -> throw $ NoPeersFound destination
                        Just source -> atomically $ writeTChan source message
                logError

    pure Relay{..}
