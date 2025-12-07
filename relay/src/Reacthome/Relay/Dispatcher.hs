module Reacthome.Relay.Dispatcher where

import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Concurrent.Chan.Unagi.Bounded (dupChan, newChan, tryRead, tryReadChan, writeChan)
import Data.Foldable (for_)
import Data.HashMap.Strict (delete, empty, insert, lookup)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Traversable (for)
import Reacthome.Relay (Uid)
import WebSockets.Connection (WebSocketSink, WebSocketSource)
import WebSockets.Options (WebSocketOptions (..))
import Prelude hiding (lookup, show)

data RelayDispatcher = RelayDispatcher
    { getSource :: Uid -> IO WebSocketSource
    , freeSource :: Uid -> IO ()
    , getSink :: Uid -> IO (Maybe WebSocketSink)
    }

makeRelayDispatcher :: (?options :: WebSocketOptions) => IO RelayDispatcher
makeRelayDispatcher = do
    !sources <- newIORef empty
    !lock <- newMVar ()

    let
        getSource !uid = do
            takeMVar lock
            !lastSources <- readIORef sources
            (!actualSources, !actualOutChan) <- case lookup uid lastSources of
                Nothing -> do
                    (!newInChan, !newOutChan) <- newChan ?options.bound
                    pure (insert uid (newInChan, 0 :: Int) lastSources, newOutChan)
                Just (!existedInChan, !count) -> do
                    !actualOutChan <- dupChan existedInChan
                    pure (insert uid (existedInChan, count + 1) lastSources, actualOutChan)
            writeIORef sources actualSources
            putMVar lock ()
            let tryReadMessage = do
                    (!element, !wait) <- tryReadChan actualOutChan
                    !message <- tryRead element
                    pure (message, wait)
            pure tryReadMessage

        freeSource !uid = do
            takeMVar lock
            !lastSources <- readIORef sources
            for_ (lookup uid lastSources) \(!inChan, !count) -> do
                let !actualSources =
                        if count == 0
                            then delete uid lastSources
                            else insert uid (inChan, count - 1) lastSources
                writeIORef sources actualSources
            putMVar lock ()

        getSink !uid = do
            !sources' <- readIORef sources
            for (lookup uid sources') \(!sink, _) -> pure do
                writeChan sink

    pure RelayDispatcher{..}
