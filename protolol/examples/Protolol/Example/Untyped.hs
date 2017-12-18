
-- |
-- Untyped implementation

module Protolol.Example.Untyped where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad as M
import Data.Function (fix)
import Data.Monoid
import Data.Functor
import Data.Time.Clock
import Prelude hiding ((>>), (>>=))

import Protolol.Example.Common

main :: IO ()
main = do
    sdup <- Duplex <$> newChan <*> newChan
    void $ concurrently
      (untypedServer sdup)
      (untypedClient (swapDuplex sdup))

untypedServer :: Duplex Message -> IO ()
untypedServer sdup = flip fix 1 (\f x -> do
    if x <= (5 :: Int)
    then do
      t1 <- getCurrentTime
      sendPingGoodBye sdup (Left Ping)
      Pong <- recvPong sdup
      t2 <- getCurrentTime
      sendChrono sdup (diffUTCTime t2 t1)
      threadDelay 1000000
      f (x + 1)
    else do
      sendPingGoodBye sdup (Right GoodBye)
    )

untypedClient :: Duplex Message -> IO ()
untypedClient sdup = fix (\f -> do
    msg <- recvPingGoodBye sdup
    case msg of
      Left Ping -> do
        sendPong sdup Pong
        x <- recvChrono sdup
        putStrLn $ "Roundtrip took: " <> show x
        f
      Right GoodBye -> pure ()
    )
