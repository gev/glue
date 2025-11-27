module Reacthome.Relay.Dispatcher where

import Control.Concurrent (modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.Chan.Unagi.Bounded (dupChan, newChan, readChan, writeChan)
import Data.HashMap.Strict (delete, empty, insert, lookup)
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
    sources <- newMVar empty

    let
        getSource uid = do
            inChan <- modifyMVar sources \sources' -> do
                case lookup uid sources' of
                    Nothing -> do
                        (inChan, _) <- newChan bound
                        pure (insert uid (inChan, 0 :: Int) sources', inChan)
                    Just (inChan, count) ->
                        pure (insert uid (inChan, count + 1) sources', inChan)
            source <- dupChan inChan
            pure
                RelaySource
                    { receiveMessage = readChan source
                    }

        freeSource uid = modifyMVar_ sources \sources' -> pure do
            case lookup uid sources' of
                Nothing -> sources'
                Just (inChan, count) -> do
                    if count == 0
                        then delete uid sources'
                        else insert uid (inChan, count - 1) sources'

        getSink uid = do
            sources' <- readMVar sources
            for (lookup uid sources') \(sink, _) ->
                pure
                    RelaySink
                        { sendMessage = writeChan sink
                        }

    pure RelayDispatcher{..}

bound :: Int
bound = 128
