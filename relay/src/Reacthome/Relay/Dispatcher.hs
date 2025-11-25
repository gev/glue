module Reacthome.Relay.Dispatcher where

import Control.Concurrent.STM (atomically, dupTChan, writeTChan)
import Control.Concurrent.STM.TChan (TChan, newTChanIO)
import Control.Exception (catch, throwIO)
import Control.Monad (void)
import Data.HashMap.Strict (empty, insert, lookup)
import Data.IORef (newIORef, readIORef)
import GHC.IORef (atomicModifyIORef'_)
import Reacthome.Relay (LazyRaw, Uid)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message (getMessageDestination)
import Prelude hiding (lookup, show)

data RelayDispatcher = RelayDispatcher
    { sendMessage :: LazyRaw -> IO ()
    , getSource :: Uid -> IO Source
    }

type Source = TChan LazyRaw

makeRelayDispatcher :: IO RelayDispatcher
makeRelayDispatcher = do
    sources <- newIORef empty

    let
        getSource uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> do
                    source <- newTChanIO
                    void $ atomicModifyIORef'_ sources $ insert uid source
                    atomically $ dupTChan source
                Just source -> atomically $ dupTChan source

        getSink uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> throwIO $ NoPeersFound uid
                Just source -> pure source

        sendMessage message =
            catch @RelayError
                do
                    let destination = getMessageDestination message
                    source <- getSink destination
                    atomically $ writeTChan source message
                logError

    pure RelayDispatcher{..}

bound :: Int
bound = 8_000
