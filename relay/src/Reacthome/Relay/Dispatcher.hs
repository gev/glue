module Reacthome.Relay.Dispatcher where

import Control.Concurrent.Chan.Unagi.Bounded (dupChan, newChan, readChan, writeChan)
import Control.Monad (void)
import Data.HashMap.Strict (delete, empty, insert, lookup)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Traversable (for)
import Reacthome.Relay (LazyRaw, Uid)
import Prelude hiding (lookup, show)

data RelayDispatcher = RelayDispatcher
    { getSource :: Uid -> IO RelaySource
    , freeSource :: Uid -> IO ()
    , getSink :: Uid -> IO (Maybe RelaySink)
    }

newtype RelaySource = RelaySource
    { receiveMessage :: IO LazyRaw
    }

newtype RelaySink = RelaySink
    { sendMessage :: LazyRaw -> IO ()
    }

makeRelayDispatcher :: IO RelayDispatcher
makeRelayDispatcher = do
    sources <- newIORef empty

    let
        getSource uid = do
            (newInChan, _) <- newChan bound
            actualInChan <- atomicModifyIORef' sources \sources' -> do
                case lookup uid sources' of
                    Nothing -> do
                        (insert uid (newInChan, 0 :: Int) sources', newInChan)
                    Just (existedInChan, count) ->
                        (insert uid (existedInChan, count + 1) sources', existedInChan)
            source <- dupChan actualInChan
            pure
                RelaySource
                    { receiveMessage = readChan source
                    }

        freeSource uid = void $ atomicModifyIORef' sources \sources' -> pure do
            case lookup uid sources' of
                Nothing -> sources'
                Just (inChan, count) -> do
                    if count == 0
                        then delete uid sources'
                        else insert uid (inChan, count - 1) sources'

        getSink uid = do
            sources' <- readIORef sources
            for (lookup uid sources') \(sink, _) ->
                pure
                    RelaySink
                        { sendMessage = writeChan sink
                        }

    pure RelayDispatcher{..}

bound :: Int
bound = 128
