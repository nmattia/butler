{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Transit.Protolol where

import Control.Monad as M
import Data.ByteString
import Data.Function (fix)
import Data.Kind
import Data.Maybe
import Data.Void
import Prelude hiding ((>>), (>>=), return)
import Transit.Types
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Store as Store

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

-- type ClientMapping = ClientMapping_ IO

type instance Func ClientMapping (S msg) = IO msg
type instance Func ClientMapping (C msg) = msg -> IO ()

type ClientM = TransitT IO ClientMapping

-------------------------------------------------------------------------------
-- Helpers for serialization
-- TODO: find a way to derive this from a Constraint
-- TODO: These may do the wrong thing for types like Either a b (or at least
-- not what the user expects)
-------------------------------------------------------------------------------

data ServerMappingF (m :: * -> *) (f :: * -> *)
data ClientMappingF (m :: * -> *) (f :: * -> *)

class TransportStore state map where
    transportStore :: (ByteString -> IO ()) -> IO ByteString -> Transport state map

-- Common instances
instance (TransportStore state any)
    => TransportStore (D :> state) any where
    transportStore send' recv = absurd :> transportStore send' recv

instance (TransportStore left any, TransportStore right any)
    => TransportStore (left :<|> right) any where
    transportStore send' recv = transportStore send' recv :<|> transportStore send' recv

instance {-# OVERLAPPABLE #-} (TransportStore (Transition state) any)
    => TransportStore state any where
    transportStore send' recv = Transition $ transportStore send' recv

-- For server

instance (TransportStore state ServerMapping, Store.Store msg)
    => TransportStore (C msg :> state) ServerMapping where
    transportStore send' recv = (Store.decodeEx <$> recv) :> transportStore send' recv

instance (TransportStore state ServerMapping, Store.Store msg)
    => TransportStore (S msg :> state) ServerMapping where
    transportStore send' recv = (send' . Store.encode) :> transportStore send' recv

-- For client

instance (TransportStore state ClientMapping, Store.Store msg)
    => TransportStore (S msg :> state) ClientMapping where
    transportStore send' recv = (Store.decodeEx <$> recv) :> transportStore send' recv

instance (TransportStore state ClientMapping, Store.Store msg)
    => TransportStore (C msg :> state) ClientMapping where
    transportStore send' recv = (send' . Store.encode) :> transportStore send' recv

------ BINARY

class TransportBinary state map where
    transportBinary
      :: (BL.ByteString -> IO ())
      -> IO BL.ByteString
      -> Transport state map

-- Common instances
instance (TransportBinary state any)
    => TransportBinary (D :> state) any where
    transportBinary send' recv = absurd :> transportBinary send' recv

instance (TransportBinary left any, TransportBinary right any)
    => TransportBinary (left :<|> right) any where
    transportBinary send' recv = transportBinary send' recv :<|> transportBinary send' recv

instance {-# OVERLAPPABLE #-} (TransportBinary (Transition state) any)
    => TransportBinary state any where
    transportBinary send' recv = Transition $ transportBinary send' recv

-- For server

instance (TransportBinary state ServerMapping, Binary.Binary msg)
    => TransportBinary (C msg :> state) ServerMapping where
    transportBinary send' recv =
      (Binary.decode <$> recv) :> transportBinary send' recv

instance (TransportBinary state ServerMapping, Binary.Binary msg)
    => TransportBinary (S msg :> state) ServerMapping where
    transportBinary send' recv = (send' . Binary.encode) :> transportBinary send' recv

-- For client

instance (TransportBinary state ClientMapping, Binary.Binary msg)
    => TransportBinary (S msg :> state) ClientMapping where
    transportBinary send' recv =
      (Binary.decode <$> recv) :> transportBinary send' recv

instance (TransportBinary state ClientMapping, Binary.Binary msg)
    => TransportBinary (C msg :> state) ClientMapping where
    transportBinary send' recv =
      (send' . Binary.encode) :> transportBinary send' recv

------ AESON

class TransportAeson state map where
    transportAeson
      :: (BL.ByteString -> IO ())
      -> IO BL.ByteString
      -> Transport state map

-- Common instances
instance (TransportAeson state any)
    => TransportAeson (D :> state) any where
    transportAeson send' recv = absurd :> transportAeson send' recv

instance (TransportAeson left any, TransportAeson right any)
    => TransportAeson (left :<|> right) any where
    transportAeson send' recv = transportAeson send' recv :<|> transportAeson send' recv

instance {-# OVERLAPPABLE #-} (TransportAeson (Transition state) any)
    => TransportAeson state any where
    transportAeson send' recv = Transition $ transportAeson send' recv

-- For server

instance (TransportAeson state ServerMapping, Aeson.FromJSON msg)
    => TransportAeson (C msg :> state) ServerMapping where
    transportAeson send' recv =
      ((fromJust . Aeson.decode) <$> recv) :> transportAeson send' recv

instance (TransportAeson state ServerMapping, Aeson.ToJSON msg)
    => TransportAeson (S msg :> state) ServerMapping where
    transportAeson send' recv =
      (send' . Aeson.encode) :> transportAeson send' recv

-- For client

instance (TransportAeson state ClientMapping, Aeson.FromJSON msg)
    => TransportAeson (S msg :> state) ClientMapping where
    transportAeson send' recv =
      ((fromJust . Aeson.decode) <$> recv) :> transportAeson send' recv

instance (TransportAeson state ClientMapping, Aeson.ToJSON msg)
    => TransportAeson (C msg :> state) ClientMapping where
    transportAeson send' recv =
      (send' . Aeson.encode) :> transportAeson send' recv
