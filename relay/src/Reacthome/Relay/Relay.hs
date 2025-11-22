module Reacthome.Relay.Relay where

import Control.Concurrent.STM (STM, readTBQueue, writeTChan)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChan)
import Control.Exception (catch, throw)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Reacthome.Relay.Error (RelayError (..), logError)
import Reacthome.Relay.Message
import StmContainers.Map (insert, lookup, newIO)
import Prelude hiding (lookup, show)

data Relay = Relay
    { sendMessage :: RelayMessage -> STM ()
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

        dispatch =
            forever $
                catch @RelayError
                    run
                    logError

        run = atomically do
            RelayMessage{from, to, content} <- readTBQueue sink
            source <-
                lookup to sources
                    >>= maybe
                        do throw $ NoPeersFound from
                        do pure
            writeTChan source $
                serializeMessage PeerMessage{peer = from, content}

    pure Relay{..}
