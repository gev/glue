module Reacthome.Relay.Dispatcher where

import Control.Concurrent.Chan.Unagi.Bounded qualified as B
import Control.Concurrent.Chan.Unagi.NoBlocking (OutChan, dupChan, newChan, writeChan)
import Control.Exception (catch, throwIO)
import Control.Monad (forever, void)
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
    , run :: IO ()
    }

type Source = OutChan LazyRaw

makeRelayDispatcher :: Int -> IO RelayDispatcher
makeRelayDispatcher bound = do
    (inChan, outChan) <- B.newChan bound
    sources <- newIORef empty

    let
        sendMessage = B.writeChan inChan

        getSource uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> do
                    (source, _) <- newChan
                    void $ atomicModifyIORef'_ sources $ insert uid source
                    dupChan source
                Just source -> dupChan source

        getSink uid = do
            sources' <- readIORef sources
            case lookup uid sources' of
                Nothing -> throwIO $ NoPeersFound uid
                Just source -> pure source

        run = forever do
            catch @RelayError
                do
                    message <- B.readChan outChan
                    let destination = getMessageDestination message
                    source <- getSink destination
                    writeChan source message
                logError

    pure RelayDispatcher{..}
