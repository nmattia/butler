{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad as M
import Control.Concurrent
import Control.Concurrent.Async
import Data.Function (fix)
import Data.Time.Clock
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Indexed
import Data.Bifunctor
import Data.Kind
import Data.Monoid
import Data.String
import Data.Void
import Data.Proxy
import Prelude hiding ((>>), (>>=), return)

data Transport p1 map where
  (:>)
    :: forall p1 p2 c map ix val
    . (p1 ~ (ix :> p2), Func map ix ~ val)
    => val -> Transport p2 map -> Transport p1 map
  (:<|>)
    :: forall p p1 p2 c
    . (p ~ (p1 :<|> p2))
    => Transport p1 c -> Transport p2 c -> Transport p c
  Transition -- Would be nice to avoid this
    :: forall p1 p2 c
    . (Transition p1 ~ p2)
    => Transport p2 c -> Transport p1 c

type family Func (map :: k) (ix :: Type) = (val :: Type)

instance Functor f => Functor (AliT f map i i) where
  fmap = imap
instance Monad m => Applicative (AliT m map i i) where
  (<*>) = iap
  pure = ireturn
instance Monad m => Monad (AliT m map i i) where
  (>>=) = flip ibind

instance (MonadIO m, i ~ j) => MonadIO (AliT m map i j) where
  liftIO :: IO a -> AliT m map i i a
  liftIO io = AliT (\s -> (,s) <$> liftIO io)

newtype AliT m map i j a where
  AliT :: (Transport i map -> m (b, Transport j map)) -> AliT m map i j b

transition :: forall j i f map. (Applicative f, Transition i ~ j) => AliT f map i j ()
transition = AliT $ \(Transition (s :: Transport j map)) -> pure ((), s)

runAliT :: AliT m map i j b
        -> Transport i map
        -> m (b, Transport j map)
runAliT (AliT io) s = io s

evalAliT :: (Functor f)
         => AliT f map i j b
         -> Transport i map
         -> f b
evalAliT (AliT io) s = fst <$> io s

execAliT :: (Functor f)
         => AliT f map i j b
         -> Transport i map
         -> f (Transport j map)
execAliT (AliT io) s = snd <$> io s

data (:>) :: k1 -> k2 -> Type
infixr :>

data (:<|>) :: k1 -> k2 -> Type
infixr 4 :<|>

type family Transition (a :: k1) = (b :: k2)

class MoveTo next left right where
  route :: Monad m => AliT m map (left :<|> right) next ()

instance MoveTo left left right where
  route = AliT $ \(s :<|> _) -> pure ((), s)

instance MoveTo right left right where
  route = AliT $ \(_ :<|> s) -> pure ((), s)

instance MoveTo foo left' right' => MoveTo foo left (left' :<|> right') where
  route = (\() -> route @foo @left' @right')
    `ibind` (AliT (\(_ :<|> s) -> pure ((),s)))

instance Applicative f => IxPointed (AliT f map) where
  ireturn x = AliT (\s -> (pure (x, s)))
instance Functor f => IxFunctor (AliT f map) where
  imap f (AliT io) = AliT $ \s -> fmap (first f) (io s)
instance Monad m => IxApplicative (AliT m map) where
  -- XXX: Can't come up with an Applicative instance for non-monads
  iap (AliT io1) (AliT io2) = AliT $ \s -> do
    (f, s') <- io1 s
    (res, s'') <- io2 s'
    return (f res, s'')
instance Monad m => IxMonad (AliT m map) where
  ibind :: (a -> AliT m map j k b) -> AliT m map i j a -> AliT m map i k b
  ibind mkA (AliT io1) = AliT $ \s -> do
    (res, s') <- io1 s
    let AliT io2 = mkA res
    io2 s'


-------------------------------------------------------------------------------
-- Helpers for Send/Receive use cases
-------------------------------------------------------------------------------

receive
  :: (Functor f, Func map ix ~ f msg)
  => AliT f map (ix :> j) j msg
receive = AliT $ \(rcv :> n) -> (,n) <$> rcv

send
  :: (Functor f,  Func map ix ~ (msg -> f ()))
  => msg
  -> AliT f map (ix :> j) j ()
send msg = AliT $ \(send' :> s) -> (,s) <$> send' msg

data C :: Type -> Type
data S :: Type -> Type
data D :: Type
type Done a = D :> a
type instance Func map D = Void -> IO ()
done :: forall s map. Transition s ~ Done s => Transport s map
done = (fix (\f -> Transition $ absurd :> f))

-- Then define a server interpretation of the protocol
data ServerMapping

type instance Func ServerMapping (S msg) = msg -> IO ()
type instance Func ServerMapping (C msg) = IO msg

type ServerM = AliT IO ServerMapping

-- Then define a client interpretation of the protocol
data ClientMapping

type instance Func ClientMapping (S msg) = IO msg
type instance Func ClientMapping (C msg) = msg -> IO ()

type ClientM = AliT IO ClientMapping

-------------------------------------------------------------------------------
-- Example implementation of client/server
-------------------------------------------------------------------------------

data Ping = Ping

data Pong = Pong

data GoodBye = GoodBye

data Message = PingMsg Ping | PongMsg Pong | GoodByeMsg GoodBye | ChronoMsg NominalDiffTime

data Start
data Quit

-- First, define the protocol
type instance Transition Start =
  S (Either Ping GoodBye) :> (C Pong :> S NominalDiffTime :> Start :<|>
                              Quit)

type instance Transition Quit = Done Quit

main :: IO ()
main = do
    sdup <- Duplex <$> newChan <*> newChan
    void $ concurrently
      (evalAliT server (serverTransport sdup))
      (evalAliT client (clientTransport (swapDuplex sdup)))

-- TODO: create a generic Transport for Binary, store, etc
serverTransport :: Duplex Message -> Transport Start ServerMapping
serverTransport c = fix $ \f ->
    Transition $
      sendPingGoodBye c :> (recvPong c :> sendChrono c :> f
                      :<|>  done)

clientTransport :: Duplex Message -> Transport Start ClientMapping
clientTransport c = fix $ \f ->
    Transition $
      recvPingGoodBye c :> (sendPong c :> recvChrono c :> f
                      :<|>  done)

data Duplex a = Duplex { sendChan :: Chan a, recvChan :: Chan a }

swapDuplex :: Duplex a -> Duplex a
swapDuplex (Duplex a b) = Duplex b a

sendDuplex :: Duplex a -> a -> IO ()
sendDuplex d msg = writeChan (sendChan d) msg

recvDuplex :: Duplex a -> IO a
recvDuplex d = readChan (recvChan d)

sendPing :: Duplex Message -> Ping -> IO ()
sendPing t msg = sendDuplex t (PingMsg msg)

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

sendGoodbye :: Duplex Message -> GoodBye -> IO ()
sendGoodbye t msg = sendDuplex t (GoodByeMsg msg)

recvGoodbye :: Duplex Message -> IO GoodBye
recvGoodbye d = do
    msg <- recvDuplex d
    case msg of
      GoodByeMsg bye -> pure bye
      _ -> throwIO (userError "")

sendChrono :: Duplex Message -> NominalDiffTime -> IO ()
sendChrono t msg = sendDuplex t (ChronoMsg msg)

recvChrono :: Duplex Message -> IO NominalDiffTime
recvChrono d = do
    msg <- recvDuplex d
    case msg of
      ChronoMsg n -> pure n
      _ -> throwIO (userError "")

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

server :: ServerM Start Quit ()
server = flip fix 1 (\f x -> do
    transition
    if x <= 400
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
    ifThenElse True b c = b
    ifThenElse False b c = c
