module Reacthome.Relay.Dispatcher where

import Control.Concurrent.Chan.Unagi.Bounded (OutChan, dupChan, newChan, writeChan)
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

type Source = OutChan LazyRaw

makeRelayDispatcher :: IO RelayDispatcher
makeRelayDispatcher = do
    sources <- newIORef empty

    let
        getSource uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> do
                    (source, _) <- newChan bound
                    void $ atomicModifyIORef'_ sources $ insert uid source
                    dupChan source
                Just source -> dupChan source

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
                    writeChan source message
                logError

    pure RelayDispatcher{..}

bound :: Int
bound = 8_000
