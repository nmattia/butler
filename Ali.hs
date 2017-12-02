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
import Control.Monad.Indexed
import Data.Kind
import Data.Monoid
import Data.String
import Data.Void
import Data.Proxy
import Prelude hiding ((>>), (>>=), return)

data Ping where
  Ping :: Ping
  deriving Show

data Pong where
  Pong :: Pong
  deriving Show

data Sender p1 map where
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

instance (i ~ j) => MonadIO (AliM i j) where
  liftIO :: IO a -> AliM i i a
  liftIO io = AliM (\s -> (s,) <$> io)

data R :: Type -> Type
data W :: Type -> Type
data D :: Type

newtype AliT m map i j a where
  AliT :: (Sender i map -> m (Sender j map, b)) -> AliT m map i j b

-- TODO: States may have kind "StateType" or (foo :> StateType) or ... so AliM
-- should be polykinded
newtype AliM i j a where
  AliM :: (Sender i ServerMapping -> IO (Sender j ServerMapping, b)) -> AliM i j b

receive :: AliM (R msg :> j) j msg
receive = AliM $ \((rcv :: IO msg) :> n) -> do
    msg <- rcv
    return (n, msg)

transition :: (Transition i ~ j) => AliM i j ()
transition = AliM $ \(SenderSkip (x :: Sender j ServerMapping)) -> pure (x,())

runPrim :: AliM i j b
        -> Sender i ServerMapping
        -> IO (Sender j ServerMapping, b)
runPrim (AliM io) s = io s

data Start
data SentPing
data Bye

data (:>) :: k1 -> k2 -> Type
infixr :>

data (:<|>) :: k1 -> k2 -> Type
infixr :<|>

type family Transition (a :: k1) = (b :: k2)

class MoveTo next left right where
  route :: AliM (left :<|> right) next ()

instance MoveTo left left right where
  route = AliM $ \(s :<|> _) -> return (s, ())

instance MoveTo right left right where
  route = AliM $ \(_ :<|> s) -> return (s, ())

instance MoveTo foo left' right' => MoveTo foo left (left' :<|> right') where
  route = (\() -> route @foo @left' @right')
    `ibind` (AliM (\(_ :<|> s) -> return (s,())))

instance IxPointed AliM where
  ireturn x = AliM (\s -> (return (s, x)))
instance IxFunctor AliM where
  imap f (AliM io) = AliM $ \s -> do
    (s', res) <- io s
    return (s', f res)
instance IxApplicative AliM where
  iap (AliM io1) (AliM io2) = AliM $ \s -> do
    (s', f) <- io1 s
    (s'', res) <- io2 s'
    return (s'', f res)

send :: msg -> AliM (W msg :> j) j ()
send msg = AliM $ \(send' :> s) -> do
    send' msg
    return (s, ())

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

type instance Transition Start =
  W Ping :> R Pong :> W Int :> (Start :<|> Bye)


-- Note: this seems to be needed in order to implement 'Senders'
type instance Transition Bye = Done Bye

main :: IO ()
main = void $ runPrim ali2 sender

-- TODO: create a generic Sender for Binary, store, etc
sender :: Sender Start ServerMapping
sender = fix $ \f ->
    SenderSkip $
      lolsend :>
      lolrecv :>
      lolsend :>
      (f :<|> done)
  where
    done :: Sender Bye ServerMapping
    done = (fix (\f -> SenderSkip $ absurd :> f))

lolsend :: Show a => a -> IO ()
lolsend x = putStrLn ("Sending " <> show x)

lolrecv :: IO Pong
lolrecv = putStrLn ("Receiving PONG" :: String) >> pure Pong

ali2 :: AliM Start Bye Pong
ali2 = flip fix 0 (\f x -> do
  transition
  send Ping
  res <- receive
  send x
  liftIO (M.return ())
  liftIO (M.return ())
  if x > 2
  then do
    route
    return res
  else do
    route
    f (x + 1)
  )
  where
    (>>) f mk = ibind (const mk) f
    (>>=) f mk = ibind mk f
    return = ireturn
    ifThenElse True b c = b
    ifThenElse False b c = c
