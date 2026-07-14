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
  , stderrTracer
  ) where

import           Cardano.Api (BlockNo (..), ChainTip (..), LocalNodeConnectInfo,
                   ShelleyGenesis (..), SlotNo (..), getLocalChainTip)

import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.Genesis as SL
import qualified Cardano.Ledger.Shelley.StabilityWindow as SL

import           Prelude

import           Control.Concurrent (ThreadId, threadDelay, throwTo)
import           Control.Exception (Exception (..), asyncExceptionFromException,
                   asyncExceptionToException)
import           Control.Exception.Safe (SomeException, try)
import           Control.Monad (void, when)
import           Control.Tracer (Tracer (..), traceWith)
import           Data.Maybe (isNothing)
import qualified Data.Time.Clock as DTC
import           System.IO (hFlush, hPutStrLn, stderr)
import           System.Process (ProcessHandle)
import           System.Timeout (timeout)

import           Testnet.Signal (hardKillProcess)

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
-- The full diagnosis is emitted through the given tracer first, so the explanation
-- can outlive the test thread (see 'stderrTracer' for the standard sink and why).
-- 'throwTo' blocks until the exception is delivered; if the test thread
-- cannot receive it (it is stuck in a foreign call or under
-- 'Control.Exception.uninterruptibleMask'), the watchdog escalates after a grace
-- period by hard-killing the node processes, so that whatever the test is blocked
-- on fails with an ordinary synchronous error instead.
--
-- Run it in a background thread (e.g. with 'Testnet.Runtime.asyncRegister_'); it is
-- stopped by cancellation like any other background resource.
chainStallWatchdog
  :: Tracer IO String -- ^ sink for the diagnosis and escalation notices
  -> ShelleyGenesis -- ^ the genesis backing the testnet, for the stall threshold
  -> LocalNodeConnectInfo -- ^ connection to the node whose chain tip is polled
  -> [ProcessHandle] -- ^ the testnet node processes, for the kill escalation
  -> ThreadId -- ^ the test thread to fail when the chain stalls
  -> IO ()
chainStallWatchdog tracer shelleyGenesis connectInfo nodeHandles testThread = do
    start <- DTC.getCurrentTime
    go start Nothing
  where
    horizon = chainForecastHorizon shelleyGenesis
    stallTimeout = chainStallTimeoutFromHorizon horizon

    go lastAdvance lastTip = do
      threadDelay pollIntervalMicros
      mTip <- queryTip
      now <- DTC.getCurrentTime
      case mTip of
        Just tip@(_, blockNo) | (snd <$> lastTip) /= Just blockNo ->
          -- Restart the stall clock only when the chain height changes: a reorg
          -- can change the tip's slot and hash without the chain growing, so
          -- those fields prove nothing about progress. Any height change counts
          -- (not just an increase): a node replaying its chain after a restart
          -- is activity rather than proof of death, and a truly dead chain
          -- freezes the height anyway.
          go now (Just tip)
        _ | now `DTC.diffUTCTime` lastAdvance >= stallTimeout ->
              reportStall lastTip
          | otherwise ->
              go lastAdvance lastTip

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
    -- Asynchronous exceptions are re-thrown ('Control.Exception.Safe.try' does
    -- not catch them): they are not query failures but this thread being told
    -- to stop (cancellation from the test teardown).
    queryTip :: IO (Maybe (SlotNo, BlockNo))
    queryTip =
      (try (timeout queryTimeoutMicros (getLocalChainTip connectInfo))
          :: IO (Either SomeException (Maybe ChainTip))) >>= \case
        Right (Just (ChainTip slotNo _ blockNo)) -> pure $ Just (slotNo, blockNo)
        Right (Just ChainTipAtGenesis) -> pure Nothing
        Right Nothing -> pure Nothing
        Left _ -> pure Nothing

    reportStall lastTip = do
      let msg = chainStallFailureMessage stallTimeout horizon lastTip
          exc = ChainStallException msg
      traceWith tracer msg
      delivered <- timeout deliveryGraceMicros $ throwTo testThread exc
      when (isNothing delivered) $ do
        traceWith tracer $ mconcat
          [ "chainStallWatchdog: could not deliver the failure to the test thread within "
          , show (deliveryGraceMicros `div` 1_000_000), "s (it is likely stuck in a foreign "
          , "call or under uninterruptibleMask, where asynchronous exceptions cannot be "
          , "received); killing the testnet nodes so that whatever it is blocked on fails instead."
          ]
        mapM_ hardKillProcess nodeHandles
        -- with the nodes dead the test thread should unblock shortly; try once more to
        -- attach the real explanation to the test failure
        void . timeout deliveryGraceMicros $ throwTo testThread exc

    pollIntervalMicros, queryTimeoutMicros, deliveryGraceMicros :: Int
    pollIntervalMicros = 5_000_000
    queryTimeoutMicros = 5_000_000
    deliveryGraceMicros = 15_000_000

-- | The standard sink for the watchdog's output: write each message to stderr and
-- flush. stderr bypasses tasty's buffered reporting, so the diagnosis is visible
-- even if the test never manages to report a result.
stderrTracer :: Tracer IO String
stderrTracer = Tracer $ \msg -> hPutStrLn stderr msg >> hFlush stderr

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
