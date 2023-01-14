{-# LANGUAGE LambdaCase #-}

import Data.Functor.Identity
import System.Environment (getArgs)

import Cardano.Tracer.Test.Forwarder
import Cardano.Tracer.Test.TestSetup

main :: IO ()
main = getArgs >>=
  \case
  [localSock, mode] ->
    let ts = TestSetup
             { tsTime         = Identity 0
             , tsThreads      = Identity 0
             , tsMessages     = Identity $ Just 0
             , tsSockInternal = Identity localSock
             , tsSockExternal = Identity ""
             , tsNetworkMagic = Identity $ NetworkMagic 42
             , tsWorkDir      = Identity "."
             }
    in case mode of
         "Initiator" -> launchForwardersSimple ts Initiator localSock 1000 2000
         "Responder" -> launchForwardersSimple ts Responder localSock 1000 2000
         _ -> err
  _ -> err
 where err = error "Usage: ./demo-forwarder /path/to/local/sock Initiator|Responder"
