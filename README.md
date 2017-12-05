# Butler Suite

The Butler suite is a set of haskell libraries for implementing state machines
and protocols.

Build with:

``` shell
$ stack build
```

This repository contains two libraries and one example:

* [transit](./transit), the library for typed states and transitions
* [protolol](./protolol), a library built on top of `transit`, focused on
  protocols
* [protolol-duplex](./protolol/examples/Duplex.hs), an example usage of
  `protolol`

## Example

Here is an example protocol:

``` haskell
-- Defined the transitions between the states (the protocol)
type instance Transition Start =
  S (Either Ping GoodBye) :> (C Pong :> S NominalDiffTime :> Start :<|>
                              Quit)
type instance Transition Quit = Done Quit
```


Here is an example transport implementation for the protocol:

``` haskell
-- | Bi-directional in process communication: send and receive
data Duplex a = Duplex { sendChan :: Chan a, recvChan :: Chan a }

-- | Help for swapping the send and receive channels
swapDuplex :: Duplex a -> Duplex a
swapDuplex (Duplex a b) = Duplex b a

-- | Server transport implementation
serverTransport :: Duplex Message -> Transport Start ServerMapping
serverTransport c = fix $ \f ->
    Transition $
      sendPingGoodBye c :> (recvPong c :> sendChrono c :> f
                      :<|>  done)

-- | Client transport implementation
clientTransport :: Duplex Message -> Transport Start ClientMapping
clientTransport c = fix $ \f ->
    Transition $
      recvPingGoodBye c :> (sendPong c :> recvChrono c :> f
                      :<|>  done)
```

Here are the server and client logic implementations:

``` haskell
server :: ServerM Start Quit ()
server = flip fix 1 (\f x -> do
    transition -- enter the transition of state 'Start'
    if x <= (5 :: Int)
    then do
      t1 <- liftIO getCurrentTime
      send (Left Ping)
      route @(C Pong :> S NominalDiffTime :> Start) -- pick a particular route
      Pong <- receive
      t2 <- liftIO getCurrentTime
      send (diffUTCTime t2 t1)
      liftIO (threadDelay 1000000)
      f (x + 1) -- loop
    else do
      send (Right GoodBye)
      route @Quit
    )

client :: ClientM Start Quit ()
client = fix (\f -> do
    transition -- enter the transition of state 'Start - client side
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
        return ()
    )
```

Here are the server and client running:

``` haskell
main :: IO ()
main = do
    sdup <- Duplex <$> newChan <*> newChan
    void $ concurrently
      (evalTransitT server (serverTransport sdup))
      (evalTransitT client (clientTransport (swapDuplex sdup)))
```
