{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protolol.Example.WebSockets where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Aeson as Aeson
import Transit.Protolol
import Transit.Types
import qualified Network.WebSockets as WS

import Protolol.Example.Common
import Protolol.Example.Typed

instance Aeson.ToJSON GoodBye
instance Aeson.FromJSON GoodBye
instance Aeson.ToJSON Ping
instance Aeson.FromJSON Ping
instance Aeson.ToJSON Pong
instance Aeson.FromJSON Pong

main :: IO ()
main = do
    let p = 8948
        h = "127.0.0.1"
    race_
      (server h p)
      (threadDelay 200000 >> client h p)

server :: String -> Int -> IO ()
server host port = WS.runServer host port $ \pconn -> do
    conn <- WS.acceptRequest pconn
    evalTransitT serverLogic $
      transportAeson
        (WS.sendBinaryData conn)
        (WS.receiveData conn)

client :: String -> Int -> IO ()
client host port = WS.runClient host port "/" $ \conn -> do
    evalTransitT clientLogic $
      transportAeson
        (WS.sendBinaryData conn)
        (WS.receiveData conn)
