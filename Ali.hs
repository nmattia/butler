{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Data.Function (fix)
import Control.Monad.Indexed
import Data.Kind
import Data.Void
import Prelude hiding ((>>), (>>=), return)


data Ping where
  Ping :: Ping
  deriving Show

data Pong where
  Pong :: Pong
  deriving Show

data Sender (p1 :: k) where
  Sender
    :: forall p1 p2 foo f
    . (p1 ~ ( foo :> p2), f ~ ToFunc foo)
    => f -> Sender p2 -> Sender p1
  SenderSkip
    :: forall p1 p2
    . (Transition p1 ~ p2)
    => Sender p2 -> Sender p1
  -- Done
    -- :: forall p1 p2
    -- . (p1 ~ p2)
    -- => Sender p1 p2
    --
    --
type family ToFunc x where
  ToFunc ('W msg) = msg -> IO ()
  ToFunc ('R msg) = IO msg

data AliM :: k1 -> k2 -> Type -> Type where
  PrimOp
    :: forall i j b
    . (Sender i -> IO (Sender j, b)) -> AliM i j b

class HasReceive m foo where
  recval :: m foo

instance (i ~ ('R msg :> j)) => HasReceive (AliM i j) msg where
  recval = PrimOp $ \(Sender recv n) -> do
    msg <- recv
    return (n, msg)

instance (Transition i ~ ('R msg :> j)) => HasReceive (AliM i j) msg where
  recval = PrimOp $ \(SenderSkip (Sender recv n)) -> do
    msg <- recv
    return (n, msg)

class HasSend m foo where
  sendal :: foo -> m ()
-- skip :: AliM

instance (i ~ ('W msg :> j)) => HasSend (AliM i j) msg where
  sendal msg = PrimOp $ \(Sender send n) -> do
    () <- send msg
    return (n, ())

instance (Transition i ~ ('W msg :> j)) => HasSend (AliM i j) msg where
  sendal msg = PrimOp $ \(SenderSkip (Sender send n)) -> do
    () <- send msg
    return (n, ())

-- sendal
  -- :: forall i j msg
  -- . (i ~ ('W msg :> j))
  -- => msg -> AliM i j ()
-- sendal msg = PrimOp $ \(Sender send n) -> do
  -- () <- send msg
  -- return (n, ())

runPrim :: AliM i j b -> Sender i -> IO (Sender j, b)
runPrim (PrimOp io) s = io s

data AliStates
  = Start
  | SentPing
  | Bye

data RW a
  = R a
  | W a

data (:>) :: RW Type -> k2 -> Type
infixr :>

type family Transition (a :: k1) = (b :: k2)

type instance Transition 'Start = 'W Ping :> 'SentPing
type instance Transition 'SentPing = 'W Void :> 'SentPing
-- type instance Transition 'SentPing = 'R Pong :> 'Bye

data HList :: [Type] -> Type where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]

instance IxPointed AliM where
  ireturn x = PrimOp (\s -> (return (s, x)))
instance IxFunctor AliM where
  imap f (PrimOp io) = PrimOp $ \s -> do
    (s', res) <- io s
    return (s', f res)
instance IxApplicative AliM where
  iap (PrimOp io1) (PrimOp io2) = PrimOp $ \s -> do
    (s', f) <- io1 s
    (s'', res) <- io2 s'
    return (s'', f res)
instance IxMonad AliM where
  ibind :: (a -> AliM j k b) -> AliM i j a -> AliM i k b
  ibind mkA (PrimOp io1) = PrimOp $ \s -> do
    (s', res) <- io1 s
    let PrimOp io2 = mkA res
    (s'', res') <- io2 s'
    return (s'', res')

main :: IO ()
main = void $ runPrim ali2 sender

sender :: Sender 'Start
sender =
  let done = (fix (\f -> SenderSkip $ Sender absurd f))
  in SenderSkip (Sender print done)

ali2 :: AliM 'Start 'SentPing ()
ali2 = sendal Ping
