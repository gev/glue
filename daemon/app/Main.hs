import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Concurrent.Chan.Unagi (newChan, readChan)
import Control.Concurrent.Chan.Unagi.Bounded qualified as B
import Control.Exception (catch)
import Control.Monad (forever, replicateM, void)
import Data.ByteString (toStrict)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Format.Numbers (prettyI)
import Data.UUID (toByteString)
import Data.UUID.V4 (nextRandom)
import GHC.IORef (atomicModifyIORef'_)
import Reacthome.Daemon.App (application)
import Reacthome.Relay.Message (RelayMessage (..), serializeMessage)
import Reacthome.Relay.Stat (RelayHits (..), RelayStat (..), RelayStatHits (..), makeRelayStat)
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)
import Web.WebSockets.Client (runWebSocketClient)
import Web.WebSockets.Error (WebSocketError)
import Prelude hiding (last)

concurrency :: Int
concurrency = 10_000

messagesPerChunk :: Int
messagesPerChunk = 1

bound :: Int
bound = 1

main :: IO ()
main = do
    peers <- replicateM concurrency nextRandom
    (peerInChans, peerOutChans) <- unzip <$> replicateM concurrency (B.newChan bound)
    (mainInChan, mainOutChan) <- newChan
    stat <- makeRelayStat
    connections <- newIORef @Int 0
    let
        port = 3003
        host = "172.16.1.1"

        run (peer, outChan, delay) = do
            threadDelay delay
            void $ atomicModifyIORef'_ connections (+ 1)
            catch @WebSocketError
                do
                    let ?inChan = mainInChan
                    let ?outChan = outChan
                    let path = "/" <> show peer
                    -- putStrLn $ "Connect to Reacthome Relay on " <> host <> ":" <> show port <> path
                    runWebSocketClient host port path $ application peer
                print
            void $ atomicModifyIORef'_ connections \n -> n - 1

        rps x1 x0 dt = fmt x1 <> " " <> " | RPS: " <> fmt ((x1 - x0) `div` dt)

        fmt = unpack . prettyI (Just '.')

        showStat = forever do
            t0 <- getTime Monotonic
            hits0 <- stat.hits
            threadDelay 1_000_000
            t1 <- getTime Monotonic
            hits1 <- stat.hits
            let !dt = fromInteger $ toNanoSecs (diffTimeSpec t1 t0) `div` 1_000_000_000
            n <- readIORef connections
            putStrLn $ "<-> " <> fmt n <> " connections"
            putStrLn $ "Tx: " <> rps hits1.tx hits0.tx dt
            putStrLn $ "Rx: " <> rps hits1.rx hits0.rx dt

        messagesPool = flip map (toStrict . toByteString <$> peers) \peer ->
            replicate messagesPerChunk $
                serializeMessage
                    RelayMessage
                        { to = peer
                        , from = peer
                        , content = encodeUtf8 "Hello Reacthome Relay ;)"
                        }

    race_
        do
            race_
                do
                    mapConcurrently_ run $ zip3 peers peerOutChans [1_000, 2_000 ..]
                showStat
        do
            race_
                do
                    threadDelay 20_000_000
                    forever do
                        for_
                            (zip peerInChans messagesPool)
                            \(inChan, messages) -> do
                                B.writeChan inChan messages
                                stat.tx.hit messagesPerChunk
                do
                    forever do
                        void $ readChan mainOutChan
                        stat.rx.hit 1
