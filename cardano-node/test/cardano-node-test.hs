{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Hedgehog.Main (defaultMain)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Test.Cardano.Config.Mainnet
#ifdef UNIX
import qualified Test.Cardano.Node.FilePermissions
#endif
import qualified Test.Cardano.Node.Json
import qualified Test.Cardano.Node.POM
import qualified Test.Cardano.Tracing.OrphanInstances.HardFork
import qualified Test.Cardano.Tracing.NewTracing.Consistency

import qualified Cardano.Crypto.Init as Crypto

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  runTests
  where
    runTests = defaultMain $
#ifdef UNIX
      [ Test.Cardano.Node.FilePermissions.tests
      ] <>
#endif
      [ Test.Cardano.Config.Mainnet.tests
      , Test.Cardano.Node.Json.tests
      , Test.Cardano.Node.POM.tests
      , Test.Cardano.Tracing.OrphanInstances.HardFork.tests
      , Test.Cardano.Tracing.NewTracing.Consistency.tests
      ]
