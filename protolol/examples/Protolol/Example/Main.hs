module Protolol.Example.Main where

import System.Environment (getArgs)

import qualified Protolol.Example.Duplex as PED
import qualified Protolol.Example.Untyped as PEU
import qualified Protolol.Example.WebSockets as PEWS

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["websockets"] -> PEWS.main
    ["untyped"] -> PEU.main
    _ -> PED.main
