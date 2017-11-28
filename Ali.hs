{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
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

import Control.Monad as M
import Data.Function (fix)
import Control.Monad.IO.Class
import Data.Kind
import Data.Monoid
import Data.String
import Data.Void
import Prelude hiding ((>>), (>>=), return)

class IxFunctor m => IxPointed m where
  ireturn :: a -> m i i a

class IxFunctor (f :: k1 -> k2 -> Type -> Type) where
  imap :: (a -> b) -> f j k a -> f j k b

class IxApply (m :: forall k1 k2. k1 -> k2 -> Type -> Type) where
  iap :: m i j (a -> b) -> m j k a -> m i k b

class IxMonad (m :: forall k1 k2. k1 -> k2 -> * -> *) where
  ibind :: (a -> m j k b) -> m i j a -> m i k b

data Ping where
  Ping :: Ping
  deriving Show

data Quit where
  Quit :: Quit
  deriving Show

data Pong where
  Pong :: Pong
  deriving Show

data Sender (p1 :: k) where
  (:>)
    :: forall p1 p2 foo f
    . (p1 ~ (foo :> p2), f ~ ToFunc foo)
    => f -> Sender p2 -> Sender p1
  (:<|>)
    :: forall p p1 p2
    . (p ~ (p1 :<|> p2))
    => Sender p1 -> Sender p2 -> Sender p
  SenderSkip
    :: forall p1 p2
    . (Transition p1 ~ p2)
    => Sender p2 -> Sender p1

-- TODO: figure out how to pass this as an argument
type family ToFunc (x :: k)
type instance  ToFunc ('W msg) = msg -> IO ()
type instance  ToFunc ('R msg) = IO msg
type instance  ToFunc 'D  = Void -> IO ()

instance Functor (AliM i i) where
  fmap = imap
instance Applicative (AliM i i) where
  (<*>) = iap
  pure = ireturn
instance Monad (AliM i i) where
  (>>=) = flip ibind

instance MonadIO (AliM (i :: k1) (i :: k1))  where
  liftIO :: IO a -> AliM i i a
  liftIO io = AliM (\s -> (s,) <$> io)

-- TODO: AliT
--
-- Note: States may have kind "StateType" or (foo :> StateType) or ... so AliM
-- should be polykinded
newtype AliM :: (forall k1 k2. k1 -> k2 -> Type -> Type) where
  AliM :: (Sender i -> IO (Sender j, b)) -> AliM i j b

class HasReceive m foo where
  recval :: m foo

instance {-# OVERLAPPING #-}
  i ~ ('R msg :> j) => HasReceive (AliM i j) msg where
  recval = AliM $ \(recv :> n) -> do
    msg <- recv
    return (n, msg)

instance (Transition i ~ ('R msg :> j)) => HasReceive (AliM i j) msg where
  recval = AliM $ \(SenderSkip (recv :> n)) -> do
    msg <- recv
    return (n, msg)

class HasSend m foo where
  sendal :: foo -> m ()

instance {-# OVERLAPPING #-}
  (i ~ ('W msg :> j)) => HasSend (AliM i j) msg where
  sendal msg = AliM $ \(send :> n) -> do
    () <- send msg
    return (n, ())

instance (Transition i ~ ('W msg :> j)) => HasSend (AliM i j) msg where
  sendal msg = AliM $ \(SenderSkip (send :> n)) -> do
    () <- send msg
    return (n, ())

runPrim :: AliM i j b -> Sender i -> IO (Sender j, b)
runPrim (AliM io) s = io s

data AliStates
  = Start
  | SentPing
  | Bye

data RW a
  = R a
  | W a
  | D

data (:>) :: k1 -> k2 -> Type
infixr :>

data (:<|>) :: k1 -> k2 -> Type
infixr :<|>

type family Transition (a :: k1) = (b :: k2)

class MoveTo next left right where
  moveTo :: AliM (left :<|> right) next ()

instance MoveTo left left right where
  moveTo = AliM $ \(s :<|> _) -> return (s, ())

-- TODO: this shouldn't be needed
instance MoveTo right left right where
  moveTo = AliM $ \(_ :<|> s) -> return (s, ())

instance (right ~ (left' :<|> right'), MoveTo foo left' right')
  => MoveTo foo left right where
  moveTo = (\() -> moveTo @foo @left' @right')
    `ibind` (AliM (\(_ :<|> s) -> return (s,())))



-- pick :: forall next past. (past ~ (next

assert :: forall s i. (s ~ i) => AliM i i ()
assert = ireturn ()

instance IxPointed AliM where
  ireturn x = AliM (\s -> (return (s, x)))
instance IxFunctor AliM where
  imap f (AliM io) = AliM $ \s -> do
    (s', res) <- io s
    return (s', f res)
instance IxApply AliM where
  iap (AliM io1) (AliM io2) = AliM $ \s -> do
    (s', f) <- io1 s
    (s'', res) <- io2 s'
    return (s'', f res)
instance IxMonad AliM where
  ibind :: (a -> AliM j k b) -> AliM i j a -> AliM i k b
  ibind mkA (AliM io1) = AliM $ \s -> do
    (s', res) <- io1 s
    let AliM io2 = mkA res
    (s'', res') <- io2 s'
    return (s'', res')

type instance Transition 'Start =
  'W Ping :> 'R Pong :> 'W Int :> ('Start :<|> 'Bye)

-- Note: this seems to be needed in order to implement 'Senders'
type instance Transition 'Bye = Done 'Bye

type Done a = 'D :> a

main :: IO ()
main = void $ runPrim ali2 sender

-- TODO: create a generic Sender for Binary, store, etc
sender :: Sender 'Start
sender = fix $ \f ->
    SenderSkip $
      lolsend :>
      lolrecv :>
      lolsend :>
      (f :<|> done)
  where
    done :: Sender 'Bye
    done = (fix (\f -> SenderSkip $ absurd :> f))

lolsend :: Show a => a -> IO ()
lolsend x = putStrLn ("Sending " <> show x)

lolrecv :: IO Pong
lolrecv = putStrLn ("Receiving PONG" :: String) >> pure Pong

ali2 :: AliM 'Start 'Bye Pong
ali2 = do
  sendal Ping
  res <- recval
  sendal 2
  moveTo @'Bye
  -- liftIO (M.return ()) :: AliM i i ()
  -- liftIO (M.return ()) :: AliM i i ()
  -- liftIO (M.return ()) :: AliM i i ()
  -- liftIO (M.return ())
  liftIO (M.return ())
  -- liftIO (M.return ())
  -- () <- return ()
  -- () <- return ()
  -- () <- return ()
  -- () <- return ()
  -- () <- return ()
  -- () <- return ()
  -- () <- return ()
  return res
  where
    (>>) f mk = ibind (const mk) f
    (>>=) f mk = ibind mk f
    return = ireturn
