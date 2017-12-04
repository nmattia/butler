{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Transit.Types
import Control.Monad as M
import Control.Concurrent
import Control.Concurrent.Async
import Data.Function (fix)
import Data.Time.Clock
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Indexed
import Data.Monoid
import Prelude hiding ((>>), (>>=), return)
import Transit.Protolol
import System.Environment (getArgs)


-- | Ping message
data Ping = Ping

-- | Pong message
data Pong = Pong

-- | GoodBye message
data GoodBye = GoodBye

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

-------------------------------------------------------------------------------
-- Untyped implementation
-------------------------------------------------------------------------------

untypedMain :: IO ()
untypedMain = do
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
  where
    ifThenElse True b _ = b
    ifThenElse False _ c = c

untypedClient :: Duplex Message -> IO ()
untypedClient sdup = fix (\f -> do
    msg <- recvPingGoodBye sdup
    case msg of
      Left Ping -> do
        sendPong sdup Pong
        x <- recvChrono sdup
        putStrLn $ "Roundtrip took: " <> show x
        f
      Right GoodBye -> do
        return ()
    )

-------------------------------------------------------------------------------
-- Typed implementation
-------------------------------------------------------------------------------

typedMain :: IO ()
typedMain = do
    sdup <- Duplex <$> newChan <*> newChan
    void $ concurrently
      (evalTransitT server (serverTransport sdup))
      (evalTransitT client (clientTransport (swapDuplex sdup)))

-- Define start and end states
data Start
data Quit

-- Defined the transitions between the states (the protocol)
type instance Transition Start =
  S (Either Ping GoodBye) :> (C Pong :> S NominalDiffTime :> Start :<|>
                              Quit)
type instance Transition Quit = Done Quit

-- Write a particular implementation of the protocol...

-- ... for the server:
serverTransport :: Duplex Message -> Transport Start ServerMapping
serverTransport c = fix $ \f ->
    Transition $
      sendPingGoodBye c :> (recvPong c :> sendChrono c :> f
                      :<|>  done)

-- ... for the client:
clientTransport :: Duplex Message -> Transport Start ClientMapping
clientTransport c = fix $ \f ->
    Transition $
      recvPingGoodBye c :> (sendPong c :> recvChrono c :> f
                      :<|>  done)

-- Write the client logic:
client :: ClientM Start Quit ()
client = fix (\f -> do
    transition
    msg <- receive
    case msg of
      Left Ping -> do
        route @(C Pong :> S NominalDiffTime :> Start)
        send Pong
        x <- receive
        liftIO (putStrLn $ "Roundtrip took: " <> show x)
        f
      Right GoodBye -> do
        route @Quit
        return ()
    )
  where
    (>>) f mk = ibind (const mk) f
    (>>=) f mk = ibind mk f

-- Write the server logic:
server :: ServerM Start Quit ()
server = flip fix 1 (\f x -> do
    transition
    if x <= (5 :: Int)
    then do
      t1 <- liftIO getCurrentTime
      send (Left Ping)
      route @(C Pong :> S NominalDiffTime :> Start)
      Pong <- receive
      t2 <- liftIO getCurrentTime
      send (diffUTCTime t2 t1)
      liftIO (threadDelay 1000000)
      f (x + 1)
    else do
      send (Right GoodBye)
      route @Quit
    )
  where
    (>>) f mk = ibind (const mk) f
    (>>=) f mk = ibind mk f
    ifThenElse True b _ = b
    ifThenElse False _ c = c

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["untyped"] -> untypedMain
    _ -> typedMain
