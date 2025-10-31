{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.SanityCheck
  ( hprop_asyncRegister_sanity_check
  , hprop_ledger_events_sanity_check
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude
import           RIO
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Data.Default.Class
import          Data.Time.Clock
import           GHC.Conc (threadStatus, ThreadStatus (..))
import           GHC.Stack

import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Runtime
import           Testnet.Start.Types

import           Hedgehog
import qualified Hedgehog.Extras as H

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
hprop_ledger_events_sanity_check :: Property
hprop_ledger_events_sanity_check = integrationRetryWorkspace 2 "ledger-events-sanity-check" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf <- mkConf tempAbsBasePath'

  let fastTestnetOptions = def
      shelleyOptions = def
        { genesisEpochLength = 100
        , genesisSlotLength = 0.1
        }

  TestnetRuntime{configurationFile, testnetNodes}
    <- createAndRunTestnet fastTestnetOptions shelleyOptions conf

  nr@TestnetNode{nodeSprocket} <- H.headM testnetNodes
  let socketPath = nodeSocketPath nr

  H.note_ $ "Sprocket: " <> show nodeSprocket
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> unFile socketPath

  !ret <- runExceptT $ handleIOExceptionsWith IOE
                   $ evalIO $ runExceptT $ foldBlocks
                       configurationFile
                       socketPath
                       FullValidation
                       [] -- Initial accumulator state
                       foldBlocksAccumulator
  case ret of
    Left (IOE e) ->
      H.failMessage callStack $ "foldBlocks failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack $ "foldBlocks failed with: " <> displayError e
    Right (Right _v) -> success


foldBlocksAccumulator
  :: Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> [LedgerEvent] -- ^ Accumulator at block i - 1
  -> IO ([LedgerEvent], FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksAccumulator _ _ allEvents _ _ =
  if any filterPoolReap allEvents
  then return (allEvents , StopFold)
  else return ([], ContinueFold)
 where
  -- We end the fold on PoolReap ledger event
  filterPoolReap :: LedgerEvent -> Bool
  filterPoolReap (PoolReap _) = True
  filterPoolReap _ = False


hprop_asyncRegister_sanity_check :: Property
hprop_asyncRegister_sanity_check = 
    withTests 1 . property $ lift $ do 

     beforeForkedThread <- getCurrentTime
     (internalState,tId) <- runResourceT $ do 
        s <- getInternalState
        (_,asyncA) <- asyncRegister_ (threadDelay 10_000_000)
        let tId = asyncThreadId asyncA
        return (s,tId)
     afterForkedThread <- getCurrentTime
     let diff' = diffUTCTime afterForkedThread beforeForkedThread
     -- The forked thread (asyncRegister_) may be some long running IO action.
     -- When the ResourceT block has finished this action should be cancelled. 
     -- Therefore we check to see that the thread is indeed cancelled when the 
     -- ResourceT block has finished.
     when (diff' >= 5) $
       throwString $ "Forked thread took too long: " <> show diff'

     stat <- threadStatus tId

     case stat of 
       ThreadFinished -> return () 
       _ -> throwString $ "Async thread not finished as expected, status: " <> show stat
     
     rMap <- readIORef internalState
     case rMap of 
      ReleaseMapClosed -> return () 
      _ -> throwString "Release map should be closed" 

