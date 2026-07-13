{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | A background watchdog that fails the test as soon as the testnet chain has
-- provably stalled forever, whatever the test happens to be waiting on.
--
-- A @cardano-testnet@ chain permanently stops producing blocks whenever no block is
-- forged for longer than the ledger view forecast horizon (see
-- 'chainForecastHorizon').
module Testnet.ChainWatchdog
  ( ChainStallException (..)
  , chainForecastHorizon
  , chainStallTimeoutFromHorizon
  , chainStallWatchdog
  ) where

import           Cardano.Api (BlockNo (..), ChainTip (..), LocalNodeConnectInfo,
                   ShelleyGenesis (..), SlotNo (..), getLocalChainTip)

import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.Genesis as SL
import qualified Cardano.Ledger.Shelley.StabilityWindow as SL

import           Prelude

import           Control.Concurrent (ThreadId, threadDelay, throwTo)
import           Control.Exception (Exception (..), SomeAsyncException, SomeException,
                   asyncExceptionFromException, asyncExceptionToException, throwIO, try)
import           Control.Monad (void, when)
import           Data.Maybe (isJust, isNothing)
import qualified Data.Time.Clock as DTC
import           System.IO (hFlush, hPutStrLn, stderr)
import           System.Process (ProcessHandle)
import           System.Timeout (timeout)

import           Testnet.Process.Run (hardKillProcess)

-- | Thrown to the test thread when the chain has irrecoverably stalled. Registered
-- as an asynchronous exception (like the exceptions of 'Control.Exception.AsyncException')
-- so that handlers for synchronous errors inside tests do not accidentally swallow it.
newtype ChainStallException = ChainStallException String

instance Show ChainStallException where
  show (ChainStallException msg) = msg

instance Exception ChainStallException where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | The ledger view forecast horizon of the chain described by the given genesis:
-- @3 * securityParam / activeSlotsCoeff@ slots (the stability window), converted to
-- wall-clock time. Nodes can only forge while the wall-clock slot is at most this far
-- past the chain tip, so a chain that has not forged for longer than this can never
-- produce a block again.
chainForecastHorizon :: ShelleyGenesis -> DTC.NominalDiffTime
chainForecastHorizon sg =
  fromIntegral horizonSlots * SL.fromNominalDiffTimeMicro (sgSlotLength sg)
  where
    horizonSlots =
      SL.computeStabilityWindow
        (SL.unNonZero $ sgSecurityParam sg)
        (SL.mkActiveSlotCoeff $ sgActiveSlotsCoeff sg)

-- | Stall detection threshold for a chain with the given forecast horizon: twice the
-- horizon (a chain quiet for longer than the horizon is already irrecoverable; the
-- factor and the 60s floor absorb block-interval variance and detection latency).
chainStallTimeoutFromHorizon :: DTC.NominalDiffTime -> DTC.NominalDiffTime
chainStallTimeoutFromHorizon horizon = max 60 (2 * horizon)

-- | Watch the chain through the given node connection and, when it has made no progress
-- for 'chainStallTimeoutFromHorizon' of the genesis, fail the given test thread with
-- a 'ChainStallException' explaining the mechanism.
--
-- The full diagnosis is printed to stderr first: stderr bypasses tasty's buffered
-- reporting, so the explanation is visible even if the test never manages to report
-- a result. 'throwTo' blocks until the exception is delivered; if the test thread
-- cannot receive it (it is stuck in a foreign call or under
-- 'Control.Exception.uninterruptibleMask'), the watchdog escalates after a grace
-- period by hard-killing the node processes, so that whatever the test is blocked
-- on fails with an ordinary synchronous error instead.
--
-- Run it in a background thread (e.g. with 'Testnet.Runtime.asyncRegister_'); it is
-- stopped by cancellation like any other background resource.
chainStallWatchdog
  :: ShelleyGenesis -- ^ the genesis backing the testnet, for the stall threshold
  -> LocalNodeConnectInfo -- ^ connection to the node whose chain tip is polled
  -> [ProcessHandle] -- ^ the testnet node processes, for the kill escalation
  -> ThreadId -- ^ the test thread to fail when the chain stalls
  -> IO ()
chainStallWatchdog shelleyGenesis connectInfo nodeHandles testThread = do
    start <- DTC.getCurrentTime
    go start Nothing
  where
    horizon = chainForecastHorizon shelleyGenesis
    stallTimeout = chainStallTimeoutFromHorizon horizon

    go lastAdvance lastPoint = do
      threadDelay pollIntervalMicros
      mPoint <- queryTip
      now <- DTC.getCurrentTime
      case mPoint of
        Just point | Just point /= lastPoint ->
          -- the tip moved: restart the stall clock
          go now (Just point)
        _ | now `DTC.diffUTCTime` lastAdvance >= stallTimeout ->
              reportStall lastPoint
          | otherwise ->
              go lastAdvance lastPoint

    -- One observation of the chain tip. 'Nothing' means nothing usable: the
    -- query failed, timed out, or the tip is still at genesis.
    --
    -- Exceptions from the query count as "no observation" rather than being
    -- propagated: one failure proves nothing, and persistent failure keeps
    -- the stall clock running until the stall timeout fires. The query has a
    -- timeout of its own because an unresponsive node (e.g. starved of CPU)
    -- can accept the connection and then never answer, which would block the
    -- polling forever.
    --
    -- Asynchronous exceptions are re-thrown: they are not query failures but
    -- this thread being told to stop (cancellation from the test teardown).
    queryTip :: IO (Maybe (SlotNo, BlockNo))
    queryTip =
      (try (timeout queryTimeoutMicros (getLocalChainTip connectInfo))
          :: IO (Either SomeException (Maybe ChainTip))) >>= \case
        Right (Just (ChainTip slotNo _ blockNo)) -> pure $ Just (slotNo, blockNo)
        Right (Just ChainTipAtGenesis) -> pure Nothing
        Right Nothing -> pure Nothing
        Left e
          | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
          | otherwise -> pure Nothing

    reportStall lastPoint = do
      let msg = chainStallFailureMessage stallTimeout horizon lastPoint
          exc = ChainStallException msg
      hPutStrLn stderr msg
      hFlush stderr
      delivered <- timeout deliveryGraceMicros $ throwTo testThread exc
      when (isNothing delivered) $ do
        hPutStrLn stderr $
          "chainStallWatchdog: could not deliver the failure to the test thread within "
            <> show (deliveryGraceMicros `div` 1_000_000) <> "s (it is likely stuck in a foreign "
            <> "call or under uninterruptibleMask, where asynchronous exceptions cannot be "
            <> "received); killing the testnet nodes so that whatever it is blocked on fails instead."
        hFlush stderr
        mapM_ hardKillProcess nodeHandles
        -- with the nodes dead the test thread should unblock shortly; try once more to
        -- attach the real explanation to the test failure
        void . timeout deliveryGraceMicros $ throwTo testThread exc

    pollIntervalMicros, queryTimeoutMicros, deliveryGraceMicros :: Int
    pollIntervalMicros = 5_000_000
    queryTimeoutMicros = 5_000_000
    deliveryGraceMicros = 15_000_000

-- | Failure message explaining why a chain that stopped extending will never recover.
-- See https://github.com/IntersectMBO/cardano-node/issues/5762
chainStallFailureMessage
  :: DTC.NominalDiffTime -- ^ the stall-detection timeout that expired
  -> DTC.NominalDiffTime -- ^ the chain's forecast horizon
  -> Maybe (SlotNo, BlockNo) -- ^ last observed chain tip, if any
  -> String
chainStallFailureMessage stallTimeout horizon lastPoint =
  unlines
    [ "The testnet chain made no progress for " <> show stallTimeout <> "."
    , case lastPoint of
        Just (SlotNo slotNo, BlockNo blockNo) ->
          "Last observed chain state: slot " <> show slotNo <> ", block " <> show blockNo <> "."
        Nothing -> "No chain state update was observed at all."
    , "The network is almost certainly stalled forever: nodes can only forge when the wall-clock"
    , "slot is at most 3 * securityParam / activeSlotsCoeff slots past the chain tip - the ledger"
    , "view forecast horizon, which is " <> show horizon <> " of wall clock for this testnet."
    , "Once no block was forged for longer than that - e.g. because node startup took too long or"
    , "the machine was too overloaded to produce a block in time - every node fails its leadership"
    , "checks and the chain can never extend again, so we fail fast instead of"
    , "hanging."
    ]
