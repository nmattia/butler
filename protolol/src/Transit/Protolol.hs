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

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Transit.Protolol where

import Transit.Types
import Control.Monad as M
import Control.Concurrent
import Control.Concurrent.Async
import Data.Function (fix)
import Data.Time.Clock
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Indexed
import Data.Kind
import Data.Monoid
import Data.Void
import Prelude hiding ((>>), (>>=), return)

-------------------------------------------------------------------------------
-- Helpers for Send/Receive use cases
-------------------------------------------------------------------------------

receive
  :: (Functor f, Func map ix ~ f msg)
  => TransitT f map (ix :> j) j msg
receive = TransitT $ \(rcv :> n) -> (,n) <$> rcv

send
  :: (Functor f,  Func map ix ~ (msg -> f ()))
  => msg
  -> TransitT f map (ix :> j) j ()
send msg = TransitT $ \(send' :> s) -> (,s) <$> send' msg

data C :: Type -> Type
data S :: Type -> Type
data D :: Type
type Done a = D :> a
type instance Func map D = Void -> IO ()

done :: forall s map. Transition s ~ Done s => Transport s map
done = (fix (\f -> Transition $ absurd :> f))

-- | Server interpretation of the protocol types
data ServerMapping

type instance Func ServerMapping (S msg) = msg -> IO ()
type instance Func ServerMapping (C msg) = IO msg

type ServerM = TransitT IO ServerMapping

-- | Client interpretation of the protocol types
data ClientMapping

type instance Func ClientMapping (S msg) = IO msg
type instance Func ClientMapping (C msg) = msg -> IO ()

type ClientM = TransitT IO ClientMapping
