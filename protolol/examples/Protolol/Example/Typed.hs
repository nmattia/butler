{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Protolol.Example.Typed where

import Control.Concurrent
import Control.Monad as M
import Control.Monad.IO.Class
import Control.Monad.Indexed
import Data.Function (fix)
import Data.Monoid
import Data.Time.Clock
import Prelude hiding ((>>), (>>=))
import Transit.Protolol
import Transit.Types

import Protolol.Example.Common

-- Define start and end states
data Start
data Quit

-- Define the transitions between the states (the protocol)
type instance Transition Start =
  S (Either Ping GoodBye) :> (C Pong :> S NominalDiffTime :> Start
                        :<|> Quit)
type instance Transition Quit = Done Quit

-- Write the clientLogic logic:
clientLogic :: ClientM Start Quit ()
clientLogic = fix $ \f -> do
    transition
    msg <- receive
    case msg of
      Left Ping -> do
        route @(C Pong :> S NominalDiffTime :> Start)
        send Pong
        x <- receive
        liftIO (putStrLn $ "Roundtrip took: " <> show x)
        f
      Right GoodBye -> do
        route @Quit
  where
    (>>) f mk = ibind (const mk) f
    (>>=) f mk = ibind mk f

-- Write the serverLogic logic:
serverLogic :: ServerM Start Quit ()
serverLogic = flip fix 1 $ \f x -> do
    transition
    if x <= (5 :: Int)
    then do
      t1 <- liftIO getCurrentTime
      send (Left Ping)
      route @(C Pong :> S NominalDiffTime :> Start)
      Pong <- receive
      t2 <- liftIO getCurrentTime
      send (diffUTCTime t2 t1)
      liftIO (threadDelay 1000000)
      f (x + 1)
    else do
      send (Right GoodBye)
      route @Quit
  where
    (>>) f mk = ibind (const mk) f
    (>>=) f mk = ibind mk f
    ifThenElse True b _ = b
    ifThenElse False _ c = c
