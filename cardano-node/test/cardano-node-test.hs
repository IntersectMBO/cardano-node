{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Hedgehog.Main (defaultMain)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

#ifdef UNIX
import qualified Test.Cardano.Node.FilePermissions
#endif
import qualified Test.Cardano.Node.Json
import qualified Test.Cardano.Node.POM
import qualified Test.Cardano.Tracing.OrphanInstances.HardFork

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  runTests
  where
    runTests = defaultMain $
#ifdef UNIX
      [ Test.Cardano.Node.FilePermissions.tests
      ] <>
#endif
      [ Test.Cardano.Node.Json.tests
      , Test.Cardano.Node.POM.tests
      , Test.Cardano.Tracing.OrphanInstances.HardFork.tests
      ]
