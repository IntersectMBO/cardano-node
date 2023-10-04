{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}

module Cardano.Testnet.Test.Cli.QuerySlotNumber
  ( hprop_querySlotNumber
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Data.Either
import           Data.Monoid (Last (..))
import qualified Data.Time.Clock as DT
import qualified Data.Time.Format as DT
import           System.Environment (getEnvironment)
import qualified System.Info as SYS

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Internal.Property as H

import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

-- | Tests @query slot-number@ cardano-cli command that it returns correct slot numbers for provided utc time
hprop_querySlotNumber :: Property
hprop_querySlotNumber = H.integrationRetryWorkspace 2 "query-slot-number" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf <- H.noteShowM $ mkConf tempAbsBasePath'

  let tempBaseAbsPath' = makeTmpBaseAbsPath $ tempAbsPath conf
      era = BabbageEra
      options = cardanoDefaultTestnetOptions
                          { cardanoNodes = cardanoDefaultTestnetNodeOptions
                          , cardanoEpochLength = 1000
                          , cardanoSlotLength = 0.02
                          , cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
                          }

  tr@TestnetRuntime
    { testnetMagic
    , poolNodes
    } <- testnet options conf
  ShelleyGenesis{sgSlotLength, sgEpochLength} <- H.noteShowM $ shelleyGenesis tr
  startTime <- H.noteShowM $ getStartTime tempAbsBasePath' tr

  let slotLength = fromNominalDiffTimeMicro sgSlotLength
      -- how many slots can the checked value differ from
      -- we have 1s precision for UTC timestamp CLI argument, so this value tells how many slots in 1s can be
      slotPrecision = round $ 1 / slotLength
      epochSize = fromIntegral sgEpochLength :: Int

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
    H.note_ "Try to retrieve slot 5s before genesis"
    testTime <- H.note . formatTime $ (-5) `DT.addUTCTime` startTime
    (result, _) <- H.runTestT $ execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWith result isLeft

  id do
    H.note_ "Retrieve slot number for the start time"
    testTime <- H.note $ formatTime startTime
    let expectedSlot = 0
    slot <- H.readNoteM =<< execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWithinTolerance slot expectedSlot slotPrecision

  id do
    H.note_ "Retrieve slot number for some delay"
    let expectedSlot = 200
        passedTime = fromIntegral expectedSlot * slotLength
    testTime <- H.note . formatTime $ passedTime `DT.addUTCTime` startTime
    slot <- H.readNoteM @Int =<< execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWithinTolerance slot expectedSlot slotPrecision

  id do
    H.note_ "Retrieve slot number at the end of epoch"
    -- that's the last slot we can look up
    -- for detailed explanation about the horizon limit see:
    -- https://github.com/input-output-hk/ouroboros-consensus/pull/62
    let expectedSlot = epochSize - 1
        passedTime = fromIntegral expectedSlot * slotLength
    testTime <- H.note . formatTime $ passedTime `DT.addUTCTime` startTime
    slot <- H.readNoteM @Int =<< execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWithinTolerance slot expectedSlot slotPrecision

  id do
    H.note_ "Try to retrieve slot beyond the horizon"
    let timeOffset = slotLength * fromIntegral epochSize * 2
    testTime <- H.note . formatTime $ timeOffset `DT.addUTCTime` startTime
    (result, _) <- H.runTestT $ execCli' execConfig
      [ "query", "slot-number"
      , "--testnet-magic", show @Int testnetMagic
      , testTime
      ]
    H.assertWith result isLeft

  -- }}}

formatTime :: DT.UTCTime -> String
formatTime = DT.formatTime DT.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

