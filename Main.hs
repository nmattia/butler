{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Prelude as P
import Prelude hiding ((>>), (>>=), return)
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad
import Data.Monoid
import Data.String

data RWList (a :: [RW *]) where
  RCons :: (IO a) -> RWList xs -> RWList (('R a) ': xs)
  WCons :: (a -> IO ()) -> RWList xs -> RWList (('W a) ': xs)
  RWNil :: RWList '[]

data RW a
  = R a
  | W a

newtype AliM past future a where
  AliM :: (RWList past -> IO (RWList future, a)) -> AliM past future a

runAliM :: AliM past future a -> RWList past -> IO (RWList future, a)
runAliM (AliM f) rw = f rw

bindAli
  :: AliM p1 p2 a
  -> (a -> AliM p2 p3 b)
  -> AliM p1 p3 b
bindAli al mkAlice = AliM $ \impl -> do
    (impl', res) <- runAliM al impl
    runAliM (mkAlice res) impl'

bindAli'
  :: AliM p1 p2 a
  -> AliM p2 p3 b
  -> AliM p1 p3 b
bindAli' al mkAlice = AliM $ \impl -> do
    (impl', _res) <- runAliM al impl
    runAliM mkAlice impl'

type family FlipRW (a :: [RW *]) where
  FlipRW (('W a) ': sub) = (('R a) ': (FlipRW sub))
  FlipRW (('R a) ': sub) = (('W a) ': (FlipRW sub))
  FlipRW '[] = '[]

returnAli
  :: a
  -> AliM p1 p1 a
returnAli x = AliM $ \impl -> pure (impl, x)

recvAliM :: AliM (('R a) :-> xs) xs a
recvAliM = AliM $ \(recv `RCons` impl) -> do
  res <- recv
  return (impl, res)

type Ali s a = AliM s Done a

sendAliM :: a -> AliM (('W a) :-> xs) xs ()
sendAliM msg = AliM $ \(send `WCons` impl) -> do
  () <- send msg
  return (impl, ())

data Ping where
  Ping :: Ping
  deriving Show

data Pong where
  Pong :: Pong
  deriving Show

type a :-> b = a : b
type Done = '[]
infixr :->

type AliceProtocol = 'W Ping :-> 'R Pong :-> Done
type BobProtocol = FlipRW AliceProtocol



type family Transition (a :: k1) = (fo :: k2)

data a :<|> b
infixr :<|>

data a :> b
infixr :>

bindAl
  :: AliM p1 p2 a
  -> (a -> AliM p2 p3 b)
  -> AliM p1 p3 b
bindAl al mkAlice = AliM $ \impl -> do
    (impl', res) <- runAliM al impl
    runAliM (mkAlice res) impl'

-- recvAl ::

data AliStates
  = Start
  | SentPing
  | Bye

type instance Transition 'Start     = 'W Ping :> 'R Pong :> 'Bye
-- type instance Transition 'SentPing  = 'R Pong :> 'Bye

type family FlipTrans (a :: k) = (f :: k) where
  FlipTrans ('W a :> sub) = ('R a :> (FlipTrans sub))
  FlipTrans ('R a :> sub) = ('W a :> (FlipTrans sub))
  FlipTrans a = a

aliceServer :: Ali AliceProtocol ()
aliceServer = do
  sendAliM Ping
  Pong <- recvAliM
  return ()
  where
    (>>=) = bindAli
    (>>) = bindAli'
    (return) = returnAli

bobServer :: Ali BobProtocol ()
bobServer = do
  Ping <- recvAliM
  sendAliM Pong
  where
    (>>=) = bindAli
    (>>) = bindAli'
    (return) = returnAli

main :: IO ()
main = do
    a2b <- newChan
    b2a <- newChan

    let sendPingA :: Ping -> IO ()
        sendPingA p = writeChan a2b (Left p)
        readPongA :: IO Pong
        readPongA = do
          Right p <- readChan b2a
          putStrLn ("Alice read: " <> show p)
          pure p
        sendPongB :: Pong -> IO ()
        sendPongB p = writeChan b2a (Right p)
        readPingB :: IO Ping
        readPingB = do
          Left p <- readChan a2b
          putStrLn ("Bob read: " <> show p)
          pure p
    void $
      concurrently
        (runAliM aliceServer (sendPingA `WCons` (readPongA `RCons` RWNil)))
        (runAliM bobServer (readPingB `RCons` (sendPongB `WCons` RWNil)))
