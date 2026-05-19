{-# LANGUAGE LambdaCase #-}

import           Cardano.Tracer.Test.Forwarder

import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  [localSock, "Initiator"] -> launchForwardersSimple Initiator localSock 2000
  [localSock, "Responder"] -> launchForwardersSimple Responder localSock 2000
  _ -> putStrLn "Usage: ./demo-forwarder /path/to/local/sock Initiator|Responder"
