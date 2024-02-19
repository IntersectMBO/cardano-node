{-# LANGUAGE LambdaCase #-}

import           Cardano.Tracer.Test.Acceptor

import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  [localSock, "Initiator", dpName] -> launchAcceptorsSimple Initiator localSock dpName
  [localSock, "Responder", dpName] -> launchAcceptorsSimple Responder localSock dpName
  _ -> putStrLn "Usage: ./demo-acceptor /path/to/local/sock Initiator|Responder Name.Of.DataPoint"
