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
    => ((msg -> IO ()) -> IO ()) -> AliM past future ()
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
  Sender :: (Transition past ~ (ev :> future)) =>
      { senderSend :: forall msg. (ev ~ 'W msg) => msg -> IO ()
      , senderNext :: Sender future
      } -> Sender past

someSender :: Sender 'Start
someSender = Sender print someSender'

someSender' :: Sender 'SentPing
someSender' = Sender print undefined -- someSender''

-- someSender'' :: Sender a
-- someSender'' = Sender undefined undefined

runAli
  :: forall p1 p2 a ev
  . (Transition p1 ~ (ev :> p2))
  => Sender p1
  -> AliM p1 p2 a
  -> IO (Sender p2, a)
runAli Sender{senderNext} (AliLift a1) = do
  res <- a1
  return (senderNext, res)
runAli snder (AliBind a1 mkAli) = do
  (snder', res) <- runAli snder a1
  runAli snder' (mkAli res)
runAli Sender{senderNext, senderSend} (AliSend mkSend) = do
  res <- mkSend senderSend
  return (senderNext, res)

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
sendal msg = AliSend $ \(send) -> do
    send msg

recval
  :: forall past future msg
  . (Transition past ~ ('R msg :> future))
  => AliM past future msg
recval = undefined

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
