{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Transit.Types where

import Control.Monad as M
import Control.Monad.IO.Class
import Control.Monad.Indexed
import Data.Bifunctor
import Data.Kind
import Prelude hiding ((>>), (>>=), return)

data Transport p1 map where
  (:>)
    :: forall p1 p2 map ix val
    . (p1 ~ (ix :> p2), Func map ix ~ val)
    => val -> Transport p2 map -> Transport p1 map
  (:<|>)
    :: forall p p1 p2 c
    . (p ~ (p1 :<|> p2))
    => Transport p1 c -> Transport p2 c -> Transport p c
  Transition -- Would be nice to avoid this extra constructor
    :: forall p1 p2 c
    . (Transition p1 ~ p2)
    => Transport p2 c -> Transport p1 c

type family Func (map :: k) (ix :: Type) = (val :: Type)

instance Functor f => Functor (TransitT f map i i) where
  fmap = imap
instance Monad m => Applicative (TransitT m map i i) where
  (<*>) = iap
  pure = ireturn
instance Monad m => Monad (TransitT m map i i) where
  (>>=) = flip ibind

-- TODO: define more instances
instance (MonadIO m, i ~ j) => MonadIO (TransitT m map i j) where
  liftIO :: IO a -> TransitT m map i i a
  liftIO io = TransitT (\s -> (,s) <$> liftIO io)

newtype TransitT m map i j a where
  TransitT :: (Transport i map -> m (b, Transport j map)) -> TransitT m map i j b

transition :: forall j i f map. (Applicative f, Transition i ~ j) => TransitT f map i j ()
transition = TransitT $ \(Transition (s :: Transport j map)) -> pure ((), s)

runTransitT
  :: TransitT m map i j b
  -> Transport i map
  -> m (b, Transport j map)
runTransitT (TransitT io) s = io s

evalTransitT
  :: (Functor f)
  => TransitT f map i j b
  -> Transport i map
  -> f b
evalTransitT (TransitT io) s = fst <$> io s

execTransitT
  :: (Functor f)
  => TransitT f map i j b
  -> Transport i map
  -> f (Transport j map)
execTransitT (TransitT io) s = snd <$> io s

data (:>) :: k1 -> k2 -> Type
infixr :>

data (:<|>) :: k1 -> k2 -> Type
infixr 4 :<|>

type family Transition (a :: k1) = (b :: k2)

class MoveTo next left right where
  route :: Monad m => TransitT m map (left :<|> right) next ()

instance MoveTo left left right where
  route = TransitT $ \(s :<|> _) -> pure ((), s)

instance MoveTo right left right where
  route = TransitT $ \(_ :<|> s) -> pure ((), s)

instance MoveTo foo left' right' => MoveTo foo left (left' :<|> right') where
  route = (\() -> route @foo @left' @right')
    `ibind` (TransitT (\(_ :<|> s) -> pure ((),s)))

instance Applicative f => IxPointed (TransitT f map) where
  ireturn x = TransitT (\s -> (pure (x, s)))
instance Functor f => IxFunctor (TransitT f map) where
  imap f (TransitT io) = TransitT $ \s -> fmap (first f) (io s)
instance Monad m => IxApplicative (TransitT m map) where
  -- XXX: Can't come up with an Applicative instance for non-monads
  iap (TransitT io1) (TransitT io2) = TransitT $ \s -> do
    (f, s') <- io1 s
    (res, s'') <- io2 s'
    return (f res, s'')
instance Monad m => IxMonad (TransitT m map) where
  ibind :: (a -> TransitT m map j k b) -> TransitT m map i j a -> TransitT m map i k b
  ibind mkA (TransitT io1) = TransitT $ \s -> do
    (res, s') <- io1 s
    let TransitT io2 = mkA res
    io2 s'
