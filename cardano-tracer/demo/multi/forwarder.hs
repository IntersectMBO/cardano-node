{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)

import Cardano.Tracer.Test.Forwarder

main :: IO ()
main = getArgs >>= \case
  [localSock, "Initiator"] -> launchForwardersSimple Initiator localSock 1000 2000
  [localSock, "Responder"] -> launchForwardersSimple Responder localSock 1000 2000
  _ -> putStrLn "Usage: ./demo-forwarder Initiator|Responder /path/to/local/sock"
