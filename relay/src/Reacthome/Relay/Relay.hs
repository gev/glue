module Reacthome.Relay.Relay where

import Control.Concurrent.Chan.Unagi.NoBlocking (OutChan, dupChan, newChan, writeChan)
import Control.Concurrent.STM (readTBQueue)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import Control.Exception (catch, throwIO)
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

type Source = OutChan LazyRaw

makeRelay :: Int -> IO Relay
makeRelay bound = do
    queue <- newTBQueueIO $ fromIntegral bound
    sources <- newIORef empty

    let
        sendMessage = atomically . writeTBQueue queue

        getSource uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> do
                    (inChan, _) <- newChan
                    void $ atomicModifyIORef'_ sources $ insert uid inChan
                    dupChan inChan
                Just source -> dupChan source

        getSink uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> throwIO $ NoPeersFound uid
                Just source -> pure source

        dispatch = forever do
            catch @RelayError
                do
                    message <- atomically $ readTBQueue queue
                    let destination = getMessageDestination message
                    source <- getSink destination
                    writeChan source message
                logError

    pure Relay{..}
