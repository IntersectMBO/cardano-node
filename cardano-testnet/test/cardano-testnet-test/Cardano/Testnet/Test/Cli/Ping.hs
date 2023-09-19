{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# HLINT ignore "Redundant id" #-}

module Cardano.Testnet.Test.Cli.Ping
  ( hprop_ping
  ) where

import           Control.Monad
import           Data.Either
import           Data.Monoid (Last (..))
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DT
import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Internal.Property as H
import           Prelude hiding (log)
import           System.Environment (getEnvironment)
import qualified System.Info as SYS

import           Cardano.Testnet
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime
import Control.Monad.IO.Class (MonadIO(liftIO))

-- | Tests @query slot-number@ cardano-cli command that it returns correct slot numbers for provided utc time
hprop_ping :: Property
hprop_ping = H.integrationRetryWorkspace 1 "ping" $ \tempAbsBasePath' -> do -- TODO smelc put 2?
  H.note_ SYS.os
  conf <- H.noteShowM $ mkConf tempAbsBasePath'

  let tempBaseAbsPath' = makeTmpBaseAbsPath $ tempAbsPath conf
      testnetOptions = BabbageOnlyTestnetOptions $ babbageDefaultTestnetOptions
        { babbageNodeLoggingFormat = NodeLoggingFormatAsJson
        }
  TestnetRuntime
    { testnetMagic
    , poolNodes
    } <- testnet testnetOptions conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  env <- H.evalIO getEnvironment
  execConfig <- H.noteShow H.ExecConfig
    { H.execConfigEnv = Last $ Just $
      [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName poolSprocket1)
      ]
      -- The environment must be passed onto child process on Windows in order to
      -- successfully start that process.
      <> env
    , H.execConfigCwd = Last $ Just tempBaseAbsPath'
    }

  id do
    json <- H.readNoteM =<< execCli' execConfig
      [ "ping", "-j", "-c", "1"
      , "--testnet-magic", show @Int testnetMagic
      ]
    -- liftIO $ forM_ (map show logs) putStrLn 
    liftIO $ putStrLn json