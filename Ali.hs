{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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

-- import qualified Prelude as P
import Prelude hiding ((>>), (>>=), return)
import Data.Kind
-- import Control.Concurrent.Async
--
-- import Control.Concurrent.Chan
import Control.Monad
-- import Data.Monoid
-- import Data.String

data Ping where
  Ping :: Ping
  deriving Show

data Pong where
  Pong :: Pong
  deriving Show

data AliM (past :: k1) (future :: k2) a where
  AliSend
    :: forall past future msg
    . (Transition past future ~ 'W msg)
    => ((msg -> IO ()) -> IO ()) -> AliM past future ()
  AliLift
    :: forall past a
    . IO a -> AliM past past a
  AliBind
    :: AliM p1 p2 a
    -> (a -> AliM p2 p3 b)
    -> AliM p1 p3 b

type family In (t :: *) (ts :: [*]) = (f :: Constraint) where
  In t1 '[t2] = (t1 ~ t2)


-- class Sends a ts where
  -- send :: In t ts => a -> t -> IO ()
-- type Sends
data Sender = Sender (forall msg. msg -> IO ())

runAli :: Sender -> AliM p1 p2 a -> IO a
runAli snder (AliLift a1) = a1
runAli snder (AliBind a1 mkAli) = runAli snder a1 >>= (runAli snder . mkAli)
runAli (Sender f) (AliSend mkSend) = mkSend f

main :: IO ()
main = pure ()

data AliStates
  = Start
  | SentPing
  | Bye

data RW a
  = R a
  | W a

data (a :: RW *) :> (b :: AliStates)
infixr :>
type family Transition (a :: k1) (b :: k2) = (c :: RW *) | c -> a b
type family Stuff (a :: k1) = (fo :: k2)

type instance Stuff ('W a :> o) = a -> IO ()
type instance Stuff ('R a :> o) = IO a

type instance Transition 'Start 'SentPing = 'W Ping
type instance Transition 'SentPing 'Bye   = 'R Pong

sendal
  :: forall past future msg
  . (Transition past future ~ 'W msg)
  => msg -> AliM past future ()
sendal msg = AliSend $ \(send) -> do
    send msg

recval
  :: forall past future msg
  . (Transition past future ~ 'R msg)
  => AliM past future msg
recval = undefined

ali2 :: AliM 'Start 'Bye ()
ali2 = do
  sendal Ping
  Pong <- recval
  return ()
  where
    (>>=) = bindAli
    (>>) = bindAli'
    (return) = returnAli

bindAli
  :: AliM p1 p2 a
  -> (a -> AliM p2 p3 b)
  -> AliM p1 p3 b
bindAli = AliBind
-- AliM $ \impl -> do
    -- (impl', res) <- runAliM al impl
    -- runAliM (mkAlice res) impl'

bindAli'
  :: AliM p1 p2 a
  -> AliM p2 p3 b
  -> AliM p1 p3 b
bindAli' al mkAlice = undefined
  -- AliM $ \impl -> do
    -- (impl', _res) <- runAliM al impl
    -- runAliM mkAlice impl'

returnAli
  :: a
  -> AliM p1 p1 a
returnAli x = undefined
-- AliM $ \impl -> pure (impl, x)
