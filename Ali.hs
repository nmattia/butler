{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad
import Data.Kind
import Prelude hiding ((>>), (>>=), return)

data Ping where
  Ping :: Ping
  deriving Show

data Pong where
  Pong :: Pong
  deriving Show

data AliM a b c where
  AliSend
    :: forall past future msg
    . (Transition past ~ ('W msg :> future))
    => msg -> AliM past future ()
  AliRecv
    :: forall past future msg
    . (Transition past ~ ('R msg :> future))
    => AliM past future msg
  AliLift
    :: forall past a
    . IO a -> AliM past past a
  AliBind
    :: forall p1 p2 p3 ev1 ev2 a b
    . ((Transition p1 ~ (ev1 :> p2))
    , (Transition p2 ~ (ev2 :> p3)))
    => AliM p1 p2 a
    -> (a -> AliM p2 p3 b)
    -> AliM p1 p3 b

data Sender bar where
  SenderW :: (Transition past ~ ('W msg :> future)) =>
      (msg -> IO ()) -> Sender future -> Sender past
  SenderR :: (Transition past ~ ('R msg :> future)) =>
      IO msg -> Sender future -> Sender past

type family NextSender (a :: k1) = (b :: k2) where
  NextSender ('W msg :> future) = Sender future
  NextSender ('R msg :> future) = Sender future
  NextSender future = ()

someSender :: Sender 'Start
someSender = SenderW (const (pure ())) someSender'

someSender' :: Sender 'SentPing
someSender' = SenderR (pure Pong) undefined

-- runAli'
  -- :: Sender p1
  -- -> AliM p1 p2 a
  -- -> IO a
-- runAli' = undefined

runAli
  :: Sender p1
  -> AliM p1 p2 a
  -> IO (Sender p2, a)
runAli snder (AliLift a1) = do
  res <- a1
  return (snder, res)
runAli snder (AliBind a1 mkAli) = do
  (snder', res) <- runAli snder a1
  runAli snder' (mkAli res)
runAli (SenderW send next) (AliSend msg) = do
  res <- send msg
  return (next, res)
runAli (SenderR recv next) AliRecv = do
  res <- recv
  return (next, res)

main :: IO ()
main = pure ()

data AliStates
  = Start
  | SentPing
  | Bye

data RW a
  = R a
  | W a

data (a :: RW *) :> (b :: k)
infixr :>
type family Transition (a :: k1) = (b :: k2)  | b -> a

type instance Transition 'Start = ('W Ping :> 'SentPing)
type instance Transition 'SentPing = ('R Pong :> 'Bye)

sendal
  :: forall past future msg
  . (Transition past ~ ('W msg :> future))
  => msg -> AliM past future ()
sendal = AliSend

recval
  :: forall past future msg
  . (Transition past ~ ('R msg :> future))
  => AliM past future msg
recval = AliRecv

ali2 :: AliM 'Start 'Bye Pong
ali2 = do
  sendal Ping
  recval
  where
    (>>=) = bindAli
    (>>) = bindAli'
    (return) = returnAli

bindAli
  :: forall p1 p2 p3 ev1 ev2 a b
  . ((Transition p1 ~ (ev1 :> p2))
  , (Transition p2 ~ (ev2 :> p3)))
  => AliM p1 p2 a
  -> (a -> AliM p2 p3 b)
  -> AliM p1 p3 b
bindAli = AliBind

bindAli'
  :: forall p1 p2 p3 ev1 ev2 a b
  . ((Transition p1 ~ (ev1 :> p2))
  , (Transition p2 ~ (ev2 :> p3)))
  => AliM p1 p2 a
  -> AliM p2 p3 b
  -> AliM p1 p3 b
bindAli' al = AliBind al . const

returnAli
  :: a
  -> AliM p1 p1 a
returnAli = AliLift . pure
