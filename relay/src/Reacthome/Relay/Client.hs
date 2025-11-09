module Reacthome.Relay.Client where

import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.ByteString.Lazy (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Network.WebSockets (Connection, receiveData, sendBinaryData, sendClose)
import Reacthome.Relay.Message (RelayMessage (RelayMessage), parseMessage, serializeMessage)
import Prelude hiding (length, splitAt, tail)

data RelayClient = RelayClient
    { start :: IO ()
    , send :: UUID -> ByteString -> IO ()
    }

makeRelayClient :: Connection -> IO RelayClient
makeRelayClient connection = do
    let
        start = do
            finally
                do
                    counter <- newIORef (0 :: Int)
                    forever do
                        message <- parseMessage <$> receiveData connection
                        counter' <- readIORef counter
                        writeIORef counter $ counter' + 1
                        when (counter' `mod` 1_000 == 0) do
                            print $ show message
                do
                    sendClose connection $ encodeUtf8 "Disconnect from Relay"

        send peer content = do
            let message = RelayMessage peer content
            sendBinaryData connection $ serializeMessage message

    pure RelayClient{..}
