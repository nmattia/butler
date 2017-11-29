{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Data.Proxy
import Data.Reflection hiding (D)
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

data Sender (p1 :: k1) (map :: k) where
  (:>)
    :: forall p1 p2 c map ix val
    . (p1 ~ (ix :> p2), Mapping map ix ~ val)
    => val -> Sender p2 map -> Sender p1 map
  (:<|>)
    :: forall p p1 p2 c
    . (p ~ (p1 :<|> p2))
    => Sender p1 c -> Sender p2 c -> Sender p c
  SenderSkip
    :: forall p1 p2 c
    . (Transition p1 ~ p2)
    => Sender p2 c -> Sender p1 c

type family Mapping (map :: k) (ix :: Type) = (val :: Type)


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

data R :: Type -> Type
data W :: Type -> Type
data D :: Type

-- TODO: AliT
--
-- Note: States may have kind "StateType" or (foo :> StateType) or ... so AliM
-- should be polykinded
data AliM :: (forall k1 k2. k1 -> k2 -> Type -> Type) where
  AliM :: (Sender i ServerMapping -> IO (Sender j ServerMapping, b)) -> AliM i j b

class HasReceive m foo where
  recval :: m foo

instance {-# OVERLAPPING #-}
  i ~ (R msg :> j) => HasReceive (AliM i j) msg where
  recval = AliM $ \((recv :: IO msg) :> n) -> do
    msg <- (recv :: IO msg)
    return (n, msg)

instance (Transition i ~ (R msg :> j)) => HasReceive (AliM i j) msg where
  recval = AliM $ \(SenderSkip (recv :> n)) -> do
    msg <- (recv :: IO msg)
    return (n, msg)

class HasSend m foo where
  sendal :: foo -> m ()

instance {-# OVERLAPPING #-}
  (i ~ (W msg :> j)) => HasSend (AliM i j) msg where
  sendal msg = AliM $ \(send :> n) -> do
    () <- send msg
    return (n, ())

instance (Transition i ~ (W msg :> j)) => HasSend (AliM i j) msg where
  sendal msg = AliM $ \(SenderSkip (send :> n)) -> do
    () <- send msg
    return (n, ())

runPrim :: AliM i j b -> Sender i ServerMapping -> IO (Sender j ServerMapping, b)
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
type Done a = D :> a


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data ServerMapping

type instance Mapping ServerMapping (W msg) = msg -> IO ()
type instance Mapping ServerMapping (R msg) = IO msg
type instance Mapping ServerMapping D = Void -> IO ()


type instance Transition 'Start =
  W Ping :> R Pong :> W Int :> ('Start :<|> 'Bye)

-- Note: this seems to be needed in order to implement 'Senders'
type instance Transition 'Bye = Done 'Bye


main :: IO ()
main = void $ runPrim ali2 sender

-- TODO: create a generic Sender for Binary, store, etc
sender :: Sender 'Start ServerMapping
sender = fix $ \f ->
    SenderSkip $
      lolsend :>
      lolrecv :>
      lolsend :>
      (f :<|> done)
  where
    done = (fix (\f -> SenderSkip $ absurd :> f))

lolsend :: Show a => a -> IO ()
lolsend x = putStrLn ("Sending " <> show x)

lolrecv :: IO Pong
lolrecv = putStrLn ("Receiving PONG" :: String) >> pure Pong

ali2 :: AliM 'Start 'Bye Pong
ali2 = flip fix 0 (\f x -> do
  sendal Ping
  res <- recval
  sendal 2
  if x < 2
  then do
    moveTo @'Bye :: AliM ('Start :<|> 'Bye) 'Bye ()
    return res
  else do
    moveTo @'Start :: AliM ('Start :<|> 'Bye) 'Start ()
    f (x + 1)
  )
  where
    (>>) f mk = ibind (const mk) f
    (>>=) f mk = ibind mk f
    return = ireturn
    ifThenElse a b c = if a then b else c
