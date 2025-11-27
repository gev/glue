module Reacthome.Relay.Dispatcher where

import Control.Concurrent (modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.Chan.Unagi.Bounded (dupChan, newChan, readChan, writeChan)
import Data.HashMap.Strict (delete, empty, insert, lookup)
import Data.Traversable (for)
import Reacthome.Relay (StrictRaw, Uid)
import Prelude hiding (lookup, show)

data RelayDispatcher = RelayDispatcher
    { getSource :: Uid -> IO RelaySource
    , freeSource :: Uid -> IO ()
    , getSink :: Uid -> IO (Maybe RelaySink)
    }

newtype RelaySource = RelaySource
    { receiveMessage :: IO StrictRaw
    }

newtype RelaySink = RelaySink
    { sendMessage :: StrictRaw -> IO ()
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
            let receiveMessage = readChan source
                {-# INLINE receiveMessage #-}
            pure
                RelaySource{..}
        {-# INLINE getSource #-}

        freeSource uid = modifyMVar_ sources \sources' -> pure do
            case lookup uid sources' of
                Nothing -> sources'
                Just (inChan, count) -> do
                    if count == 0
                        then delete uid sources'
                        else insert uid (inChan, count - 1) sources'
        {-# INLINE freeSource #-}

        getSink uid = do
            sources' <- readMVar sources
            for (lookup uid sources') \(sink, _) ->
                let sendMessage = writeChan sink
                    {-# INLINE sendMessage #-}
                 in pure
                        RelaySink{..}
        {-# INLINE getSink #-}

    pure RelayDispatcher{..}

bound :: Int
bound = 128
