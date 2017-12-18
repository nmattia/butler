-------------------------------------------------------------------------------
-- Typed implementation
-------------------------------------------------------------------------------
--
module Protolol.Example.Duplex where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Function (fix)
import Data.Functor
import Transit.Protolol
import Transit.Types

import Protolol.Example.Common
import Protolol.Example.Typed

main :: IO ()
main = do
    sdup <- Duplex <$> newChan <*> newChan
    void $ concurrently
      (evalTransitT serverLogic (serverTransport sdup))
      (evalTransitT clientLogic (clientTransport (swapDuplex sdup)))


-- Write a particular implementation of the protocol...

-- ... for the server:
serverTransport :: Duplex Message -> Transport Start ServerMapping
serverTransport c = fix $ \f ->
    Transition $
      sendPingGoodBye c :> (recvPong c :> sendChrono c :> f
                      :<|>  done)

-- ... for the client:
clientTransport :: Duplex Message -> Transport Start ClientMapping
clientTransport c = fix $ \f ->
    Transition $
      recvPingGoodBye c :> (sendPong c :> recvChrono c :> f
                      :<|>  done)
