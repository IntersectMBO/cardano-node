{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.LedgerEvents.SanityCheck
  ( hprop_checkLedgerStateCondition_condition_met
  , hprop_checkLedgerStateCondition_temination_epoch
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.Text as Text
import           GHC.IO.Exception (IOException)
import           GHC.Stack (callStack)
import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H

import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

newtype AdditionalCatcher
  = IOE IOException
  deriving Show

-- Ledger events can be emitted upon the application of the various ledger rules.
-- Event definition example: https://github.com/input-output-hk/cardano-ledger/blob/afedb7d519761ccdd9c013444aa4b3e0bf0e68ef/eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs#L198
-- Event emission: https://github.com/input-output-hk/cardano-ledger/blob/afedb7d519761ccdd9c013444aa4b3e0bf0e68ef/eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs#L389
-- We can directly access these events via `foldBlocks` exposed by cardano-api. In the normal operation of a node, these events are ignored
-- (see: https://github.com/input-output-hk/ouroboros-consensus/commit/c1abf51948a673a2bbd540e5b7929ce1f07c108e#diff-4274b4c9494fc060b0980695df1b5de3412eccd31cd10c77836ef5bc66e40dd8R123) however a node's client
-- that is requesting blocks can reconstruct the ledger state and access the ledger events via `tickThenApplyLedgerResult`. This is what
-- `foldBlocks` does. Below is a simple test that illustrates `foldBlocks` pattern matching on the RetiredPools event (https://github.com/input-output-hk/cardano-ledger/blob/afedb7d519761ccdd9c013444aa4b3e0bf0e68ef/eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/PoolReap.hs#L177).
-- This sets the stage for more direct testing of clusters allowing us to avoid querying the node, dealing with serialization to and from disk,
-- setting timeouts for expected results etc.
hprop_checkLedgerStateCondition_condition_met :: Property
hprop_checkLedgerStateCondition_condition_met = H.integrationWorkspace "Ledger state condition met" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf <- H.noteShowM $  mkConf tempAbsBasePath'

  let fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        }

  !testnetRuntime
    <- cardanoTestnet fastTestnetOptions conf
  NodeRuntime{nodeSprocket} <- H.headM $ poolRuntime <$> poolNodes testnetRuntime
  let socketName' = IO.sprocketName nodeSprocket
      socketBase = IO.sprocketBase nodeSprocket -- /tmp
      socketPath = socketBase </> socketName'

  H.note_ $ "Sprocket: " <> show nodeSprocket
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  !ret <- runExceptT
            $ handleIOExceptT IOE
            $ runExceptT
            $ checkLedgerStateCondition
                (File $ configurationFile testnetRuntime)
                (File socketPath)
                FullValidation
                0
                (const ConditionMet)

  case ret of
    Left (IOE e) ->
      H.failMessage callStack $ "checkLedgerStateCondition failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack $ "checkLedgerStateCondition failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right v) -> if ConditionMet == v
                       then success
                       else H.failMessage callStack $ "Condition not met: " <> show v



hprop_checkLedgerStateCondition_temination_epoch :: Property
hprop_checkLedgerStateCondition_temination_epoch = H.integrationWorkspace "Ledger state termination epoch reaced" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf <- H.noteShowM $  mkConf tempAbsBasePath'

  let fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoSlotLength = 0.1
        }

  !testnetRuntime
    <- cardanoTestnet fastTestnetOptions conf
  NodeRuntime{nodeSprocket} <- H.headM $ poolRuntime <$> poolNodes testnetRuntime
  let socketName' = IO.sprocketName nodeSprocket
      socketBase = IO.sprocketBase nodeSprocket -- /tmp
      socketPath = socketBase </> socketName'
  let terminationEpoch = 1
  H.note_ $ "Sprocket: " <> show nodeSprocket
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  !ret <- runExceptT
            $ handleIOExceptT IOE
            $ runExceptT
            $ checkLedgerStateCondition
                (File $ configurationFile testnetRuntime)
                (File socketPath)
                FullValidation
                terminationEpoch
                (const ConditionNotMet)
  case ret of
    Left (IOE e) ->
      H.failMessage callStack $ "checkLedgerStateCondition failed with: " <> show e
    Right (Left (FoldBlocksApplyBlockError (TerminationEpochReached termEpoch))) ->
      termEpoch === terminationEpoch
    Right (Left e) ->
      H.failMessage callStack $ "checkLedgerStateCondition failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right v) ->
      H.failMessage callStack $ "checkLedgerStateCondition unexpectedly succeeded with: " <> show v

