{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Hedgehog.Main (defaultMain)
import           Main.Utf8 (withUtf8)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import           System.IO.CodePage (withCP65001)

import qualified Test.Cardano.Config.Mainnet
#ifdef UNIX
import qualified Test.Cardano.Node.FilePermissions
#endif
import qualified Test.Cardano.Node.Json
import qualified Test.Cardano.Node.POM
import qualified Test.Cardano.Node.TopLevel
import qualified Test.Cardano.Tracing.ForgingStats
import qualified Test.Cardano.Tracing.NewTracing.Consistency

import qualified Cardano.Crypto.Init as Crypto

main :: IO ()
main = withCP65001 $ withUtf8 $ do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
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
      , Test.Cardano.Node.TopLevel.tests
      , Test.Cardano.Tracing.ForgingStats.tests
      , Test.Cardano.Tracing.NewTracing.Consistency.tests
      ]
