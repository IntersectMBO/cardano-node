{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Cardano.Prelude
import           Hedgehog.Main (defaultMain)

#ifdef UNIX
import qualified Test.Cardano.Node.FilePermissions
#endif
import qualified Test.Cardano.Node.Json
import qualified Test.Cardano.Node.POM

main :: IO ()
main = defaultMain
  ( [ Test.Cardano.Node.Json.tests
    , Test.Cardano.Node.POM.tests
    ]
#ifdef UNIX
      ++ [Test.Cardano.Node.FilePermissions.tests]
#endif
  )
