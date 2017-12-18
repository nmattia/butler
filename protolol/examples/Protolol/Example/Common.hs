{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protolol.Example.Common where

import Control.Concurrent
import Control.Exception
import Data.Functor
import Data.Functor.Contravariant
import Data.Store
import Data.Time.Clock
import GHC.Generics (Generic)
import Prelude hiding ((>>), (>>=))


-- | Ping message
data Ping = Ping deriving Generic
instance Store Ping

-- | Pong message
data Pong = Pong deriving Generic
instance Store Pong

-- | GoodBye message
data GoodBye = GoodBye deriving Generic
instance Store GoodBye

instance Store NominalDiffTime where
    size = contramap fromEnum size
    peek = toEnum <$> peek
    poke = poke . fromEnum

-- | The sum type used by our communication channel ('Chan Message')
data Message
  = PingMsg Ping
  | PongMsg Pong
  | GoodByeMsg GoodBye
  | ChronoMsg NominalDiffTime

-- | Two-directional communication: send and receive
data Duplex a = Duplex { sendChan :: Chan a, recvChan :: Chan a }

-- | Help for swapping the send and receive channels
swapDuplex :: Duplex a -> Duplex a
swapDuplex (Duplex a b) = Duplex b a

-- Helpers for sending messages over a 'Duplex'

sendDuplex :: Duplex a -> a -> IO ()
sendDuplex d msg = writeChan (sendChan d) msg

recvDuplex :: Duplex a -> IO a
recvDuplex d = readChan (recvChan d)

sendPingGoodBye :: Duplex Message -> Either Ping GoodBye -> IO ()
sendPingGoodBye t msg = sendDuplex t $
  case msg of
      Left ping -> PingMsg ping
      Right bye -> GoodByeMsg bye

recvPingGoodBye :: Duplex Message -> IO (Either Ping GoodBye)
recvPingGoodBye d = do
    msg <- recvDuplex d
    case msg of
      PingMsg ping -> pure (Left ping)
      GoodByeMsg bye -> pure (Right bye)
      _ -> throwIO (userError "")

sendPong :: Duplex Message -> Pong -> IO ()
sendPong t msg = sendDuplex t (PongMsg msg)

recvPong :: Duplex Message -> IO Pong
recvPong d = do
    msg <- recvDuplex d
    case msg of
      PongMsg pong -> pure pong
      _ -> throwIO (userError "")

sendChrono :: Duplex Message -> NominalDiffTime -> IO ()
sendChrono t msg = sendDuplex t (ChronoMsg msg)

recvChrono :: Duplex Message -> IO NominalDiffTime
recvChrono d = do
    msg <- recvDuplex d
    case msg of
      ChronoMsg n -> pure n
      _ -> throwIO (userError "")
