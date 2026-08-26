{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
-----------
-- aeson --
-----------
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
-----------
-- async --
-----------
import Control.Concurrent.Async qualified as Async
----------------
-- bytestring --
----------------
import Data.ByteString.Char8 qualified as BS8
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
-------------------------
-- cardano-ledger-core --
-------------------------
import Cardano.Ledger.Coin qualified as L
------------------
-- cardano-node --
------------------
import Cardano.Node.Configuration.POM
  ( parseNodeConfigurationFP
  , makeNodeConfiguration
  , defaultPartialNodeConfiguration
  , PartialNodeConfiguration(..)
  , NodeConfiguration
  , ncProtocolConfig
  )
import Cardano.Node.Handlers.Shutdown (ShutdownConfig(..))
import Cardano.Node.Protocol.Cardano (mkSomeConsensusProtocolCardano)
import Cardano.Node.Protocol.Types (SomeConsensusProtocol(..))
import Cardano.Node.Types
  ( ConfigYamlFilePath(..)
  , KESSource(..)
  , NodeProtocolConfiguration(..)
  , ProtocolFilepaths(..)
  )
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
-------------
-- network --
-------------
import Network.Socket qualified as Socket
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..))
---------------------------------
-- ouroboros-network:framework --
---------------------------------
import Ouroboros.Network.IOManager (withIOManager)
---------
-- stm --
---------
import Control.Concurrent.STM qualified as STM
----------
-- text --
----------
import Data.Text qualified as Text
------------------
-- transformers --
------------------
import Control.Monad.Trans.Except (runExceptT)
------------------
-- pull-fiction --
------------------
import Cardano.Benchmarking.PullFiction.Config.Raw qualified as Raw
import Cardano.Benchmarking.PullFiction.Config.Runtime qualified as Runtime
import Cardano.Benchmarking.PullFiction.Config.Validated qualified as Validated
import Cardano.Benchmarking.PullFiction.WorkloadRunner (runWorkload)
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Block qualified as Block
import Cardano.Benchmarking.TxCentrifuge.NodeToClient qualified as N2C
import Cardano.Benchmarking.TxCentrifuge.NodeToClient.TxIdSync
  qualified as TxIdSyncN2C
import Cardano.Benchmarking.TxCentrifuge.NodeToNode qualified as N2N
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.KeepAlive
  qualified as KeepAlive
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxIdSync
  qualified as TxIdSyncN2N
import Cardano.Benchmarking.TxCentrifuge.NodeToNode.TxSubmission
  qualified as TxSubmission
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
import Cardano.Benchmarking.TxCentrifuge.Tracing qualified as Tracing
import Cardano.Benchmarking.TxCentrifuge.TxAssembly qualified as TxAssembly

--------------------------------------------------------------------------------
-- Era
--------------------------------------------------------------------------------

-- | The Shelley-based era this generator builds and submits transactions in.
-- Every era-specific type in this module is written in terms of 'Era', and
-- 'era' below is its value-level witness. Moving to another era means changing
-- these two definitions (and wiring the era through the rest of the pipeline).
type Era = Api.ConwayEra

-- | Value-level witness of 'Era', for cardano-api builders that take an
-- 'Api.ShelleyBasedEra' argument (such as 'TxAssembly.buildTx').
era :: Api.ShelleyBasedEra Era
era = Api.ShelleyBasedEraConway

--------------------------------------------------------------------------------

main :: IO ()
main = do

  -- Loand and validate config.
  -----------------------------

  (isDryRun, validated, codecConfig, networkId, networkMagic, tracers) <-
    loadConfig

  -- Dry run: config, protocol and on-chain fund discovery have been validated,
  -- so exit before creating resources or generating any traffic.
  when isDryRun $ do
    hPutStrLn stderr "Dry run OK: config and funds validated, exiting."
    exitSuccess

  -- Callbacks / handlers.
  ------------------------

  -- From 'String' (address) and 'Int' (port) to 'AddrInfo'.
  let resolveAddr ip port = do
        let hints = Socket.defaultHints
              { Socket.addrSocketType = Socket.Stream
              , Socket.addrFamily     = Socket.AF_INET
              }
        addrs <- Socket.getAddrInfo
          (Just hints)
          (Just ip)
          (Just (show port))
        case addrs of
          []    -> die $ "Cannot resolve target: " ++ ip ++ ":" ++ show port
          (a:_) -> pure a

  -- Builder factory passed to 'Runtime.resolve'. Returns a 'BuilderHandle'.
  -- Receives builder's zero-based index and name and the opaque builder config.
  let mkBuilder builderIndex builderName rawBuilder = do
        -- Interpret the opaque builder config into a concrete builder, with its
        -- destination signing key and address already resolved.
        builder <- interpretBuilder networkId builderIndex rawBuilder
        -- Announce the destination addresses so an operator knows which
        -- addresses to fund and can inspect their UTxOs. One line per builder.
        hPutStrLn stderr $
          "Builder " ++ builderName ++ ": destination addresses "
          ++ Text.unpack
               (Text.intercalate
                 ", "
                 (map (Api.serialiseAddress . snd) (destinations builder))
               )
        -- This builder owns its loop: it pulls a fixed 'inputsPerTx' batch,
        -- builds a transaction, and either publishes it or, when the batch is
        -- unbuildable (an all-dust batch whose input value does not cover the
        -- fee), drops it and stays up. The engine hands over 'api', a sealed
        -- window onto this builder's pipe and recycler.
        pure $ Runtime.BuilderHandle $ \api -> forever $ do
          inputFunds <- Runtime.baTakeInputs api (inputsPerTx builder)
          let buildTxAns = TxAssembly.buildTx
                             era
                             (destinations builder)
                             inputFunds (outputsPerTx builder)
                             (L.Coin (fee builder))
          case buildTxAns of
            -- A per-batch value failure: these particular inputs cannot cover
            -- the fee plus one valid output each (the only 'buildTx' failure
            -- that depends on the inputs). Abandon the batch via 'baDropInputs'
            -- (the engine neither recycles nor enqueues it), then emit the
            -- TxCentrifuge.Builder.InputsDropped trace. Action first, then the
            -- trace, as everywhere else.
            --
            -- Whole-batch, not per-input, on purpose: 'buildTx' sums ALL
            -- inputs, so a batch fails only when they are COLLECTIVELY worth at
            -- most the fee. A small input among good ones is summed in and
            -- spent, never dropped. Filtering out every input with value <= fee
            -- would instead discard dust that is productively swept into a tx
            -- today.
            Left (TxAssembly.InsufficientValue reason) -> do
              Runtime.baDropInputs api inputFunds
              Tracing.traceWith
                (Tracing.trBuilder tracers)
                (Tracing.BuilderInputsDropped builderName inputFunds reason)
            -- The remaining failures do not depend on the batch, so every batch
            -- would hit them and dropping would spin forever: fail loudly.
            -- 'InvalidInput' is a bad builder argument (already guarded in
            -- 'interpretBuilder', so defensive here). 'LedgerFailure' is an
            -- opaque cardano-api construction error.
            Left (TxAssembly.InvalidInput reason) ->
              die $ "TxAssembly.buildTx: invalid builder parameters: " ++ reason
            Left (TxAssembly.LedgerFailure reason) ->
              die $ "TxAssembly.buildTx: ledger construction failed: " ++ reason
            Right (tx, outputFunds) -> do
              -- The TxID is needed for the "on_confirm" recycling strategy.
              let txId = Api.getTxId (Api.getTxBody tx)
              -- Trace the newly built transaction
              -- (TxCentrifuge.Builder.NewTx), purely a construction event, no
              -- pipe/queue info.
              Tracing.traceWith
                (Tracing.trBuilder tracers)
                (Tracing.BuilderNewTx
                  builderName txId
                  -- The destination addresses.
                  (map snd (destinations builder))
                  inputFunds outputFunds
                )
              Runtime.baAddPayload api txId tx inputFunds outputFunds

  -- Pipe-events factory passed to 'Runtime.resolve'. Returns a 'PipeHandle'
  -- Given builder's zero-based index and name create the handlers for the four
  -- pipe's queue mechanics (TxCentrifuge.Pipe.*): payloads added/removed (with
  -- the resulting queue depth) and inputs added/removed
  -- (TxCentrifuge.Pipe.Inputs{Enqueued,Dequeued}, with the inputs themselves
  -- and the resulting queue depth).
  let mkPipeHandle _pipeIndex builderName =
        pure Runtime.PipeHandle
          { Runtime.phOnInputsEnqueued = \inputs depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipeInputsEnqueued builderName depth inputs)
          , Runtime.phOnInputsDequeued = \inputs depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipeInputsDequeued builderName depth inputs)
          , Runtime.phOnPayloadEnqueued = \key depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipePayloadEnqueued builderName depth key)
          , Runtime.phOnPayloadDequeued = \key depth ->
              Tracing.traceWith (Tracing.trPipe tracers)
                (Tracing.PipePayloadDequeued builderName depth key)
          }

  -- Recovery factory used by 'mkRecyclerHandle'. When the workload's builder
  -- configured a "recovery" (carrying the observer whose orphan events trigger
  -- it and the input source that rebuilds the queued inputs), build the
  -- recovery action the forwarder runs on an orphan: query the UTxOs currently
  -- at the builder's destination address through the source's socket, whose
  -- result reseeds the builder's input queue (the recycler applies it via its
  -- reset, which drops the stale queued inputs and the stale queued payloads
  -- first). The source is independent of the observer, which stays a shared
  -- entity the recovery never modifies: the orphan signal can come from any
  -- observer type or node while the recovery queries a (typically local)
  -- socket, and builders with and without a recovery can name the same
  -- observer. Only a "utxo_query" source can rebuild inputs (a static file
  -- cannot reflect the chain after a rollback). The socket is probed once here
  -- so a wrong "socket_path" dies at startup, not at the first rollback. At
  -- runtime a failed query reports and retries after a delay until it succeeds:
  -- the orphan event that triggered it is already consumed, so giving up would
  -- swallow this rollback's recovery and leave the queue poisoned until the
  -- next one. The query returns failures as 'Left' and never throws (see
  -- 'UTxOQuery.queryUTxOsAtAddresses'), so the retry loop swallows no
  -- exceptions and a genuine exception still ends the run as everywhere.
  let mkRecover builderIndex builderName =
        case Map.lookup builderName (Validated.workloads validated) of
          Nothing -> pure Nothing
          Just wl ->
            case Raw.builderRecovery (Validated.builder wl) of
              Nothing -> pure Nothing
              Just recovery -> do
                -- The source reference is validated, the lookup is total.
                let sourceName = Raw.recoverySource recovery
                    rawSource  =
                      Validated.inputSources validated Map.! sourceName
                socketPath <- case interpretInputSource rawSource of
                  Left err -> die $
                    "input_sources." ++ sourceName ++ ": " ++ err
                  Right (UTxOQuerySource path) -> pure path
                  Right (GenesisKeysSource _)  -> die $
                    "Builder " ++ builderName ++ ": recovery source "
                    ++ show sourceName ++ " is a static file and cannot"
                    ++ " rebuild inputs after a reset, use a \"utxo_query\""
                    ++ " source"
                builder <- interpretBuilder
                             networkId builderIndex (Validated.builder wl)
                -- Startup probe: only connectivity matters, an empty result is
                -- fine (the address fills up as this builder's transactions
                -- confirm).
                eProbe <- Fund.discoverFundsAtAddresses
                            networkId socketPath
                            (destinations builder)
                case eProbe of
                  Left err -> die $
                    "Builder " ++ builderName ++ ": recovery socket ("
                    ++ socketPath ++ "): " ++ err
                  Right _ -> pure ()
                pure $ Just $
                  let attempt = do
                        eFunds <- Fund.discoverFundsAtAddresses
                                    networkId socketPath
                                    (destinations builder)
                        case eFunds of
                          Left err -> do
                            hPutStrLn stderr $
                              "Builder " ++ builderName
                              ++ ": recovery query failed"
                              ++ " (retrying in 1s): " ++ err
                            threadDelay 1_000_000
                            attempt
                          Right funds -> pure funds
                  in attempt

  -- Recycler-events factory passed to 'Runtime.resolve'. Returns a
  -- 'RecyclerHandle'. Given the builder's zero-based index and name, creates
  -- the handlers that trace when a payload enters the backlog
  -- (TxCentrifuge.Recycler.AddToBacklog), when its inputs are added to the
  -- pipe (TxCentrifuge.Recycler.AddToPipe), and the optional recovery action
  -- (see 'mkRecover'). The pipe and recycler are both named by the workload
  -- here (one of each per workload).
  let mkRecyclerHandle recyclerIndex builderName = do
        recover <- mkRecover recyclerIndex builderName
        pure Runtime.RecyclerHandle
          { Runtime.rhOnAddToBacklog = \key consumed outputs backlog ->
              Tracing.traceWith (Tracing.trRecycler tracers)
                (Tracing.RecyclerAddToBacklog
                  builderName backlog key consumed outputs)
          , Runtime.rhOnAddToPipe = \inputs backlog ->
              Tracing.traceWith (Tracing.trRecycler tracers)
                (Tracing.RecyclerAddToPipe builderName builderName backlog inputs)
            -- The trace keeps the dropped payloads by txId (their key), not
            -- the full transactions.
          , Runtime.rhOnReset = \droppedInputs droppedPayloads fresh backlog ->
              Tracing.traceWith (Tracing.trRecycler tracers)
                (Tracing.RecyclerReset
                  builderName builderName
                  backlog (map fst droppedPayloads) droppedInputs fresh)
          , Runtime.rhRecover = recover
          }

  -- Observer factory passed to 'Runtime.resolve'. Returns an 'ObserverHandle'.
  -- For each observer in the config creates an N2N or N2C connection for
  -- transaction confirmation tracking.
  -- Takes the 'IOManager' as first argument (partial-applied below).
  let mkObserver ioManager _observerIndex observerName rawObserver = do
        -- From JSON/Aeson.Value to the cardano-node specific observer.
        observer <- case interpretObserver rawObserver of
          Left err -> die $ "Observer " ++ observerName ++ ": " ++ err
          Right o  -> pure o
        -- Observer announce loop: dup the confirmation broadcast and log every
        -- confirmed/orphaned tx (TxCentrifuge.Observer.Announce), decoupled from
        -- the pipe and recycling. Runs alongside the connection in 'ohRun'.
        -- TODO: this is all-chain — it logs every confirmed tx the observer sees
        -- on the chain, not just this generator's transactions (the broadcast is
        -- unfiltered). Filter to our own txIds once Main tracks its in-flight set.
        let announceLoop broadcast = do
              chan <- STM.atomically $ STM.dupTChan broadcast
              forever $ do
                eitherBlockTx <- STM.atomically $ STM.readTChan chan
                let (isOrphan, txId) = case eitherBlockTx of
                      Left  blockTx -> (True,  Block.blockTxId blockTx)
                      Right blockTx -> (False, Block.blockTxId blockTx)
                Tracing.traceWith (Tracing.trObserver tracers)
                  (Tracing.ObserverAnnounce observerName txId isOrphan)
        case observer of
          -- N2N: ChainSync (headers) + BlockFetch (blocks) + KeepAlive.
          --------------------------------------------------------------
          NodeToNode addr port depth -> do
            syncState <- TxIdSyncN2N.emptyState
              TxIdSyncN2N.Config
                { TxIdSyncN2N.confirmationDepth = depth }
            keepAlive <- KeepAlive.keepAliveClient 10
            let clients = N2N.emptyClients
                  { N2N.clientChainSync  =
                      Just $ TxIdSyncN2N.chainSyncClient  syncState
                  , N2N.clientBlockFetch =
                      Just $ TxIdSyncN2N.blockFetchClient syncState
                  , N2N.clientKeepAlive  = Just keepAlive
                  }
            addrInfo <- resolveAddr addr port
            pure Runtime.ObserverHandle
              { Runtime.ohRun =
                  -- The announce loop runs alongside the connection and is
                  -- cancelled when the connection ends.
                  Async.withAsync
                    (announceLoop (TxIdSyncN2N.stateBroadcast syncState)) $ \announceAsync -> do
                      -- Link the announce loop so its failure aborts the
                      -- observer instead of being silently swallowed.
                      Async.link announceAsync
                      result <- N2N.connect
                        ioManager codecConfig networkMagic tracers
                        addrInfo clients
                      case result of
                        Left err ->
                          die $ "observer " ++ observerName ++ ": " ++ err
                        Right () -> pure ()
              , Runtime.ohSubscribe = do
                  chan <- STM.atomically $
                    STM.dupTChan (TxIdSyncN2N.stateBroadcast syncState)
                  -- Reduce each broadcast BlockTx to its TxId (the recycle key).
                  -- TODO: own-traffic filter. This adapter is where a future
                  -- filter should drop foreign events (txIds this generator
                  -- never built) before any forwarder, recovery query or reset
                  -- sees them: an in-flight txId set filled at build time and
                  -- pruned at confirm/orphan. Whether it lives here or as a
                  -- generic facility in pull-fiction is an open decision. Until
                  -- then, keyed resets (on_confirm) are gated by the recycler's
                  -- backlog and the optimistic strategies reset on any settled
                  -- orphan.
                  pure $ do
                    eitherBlockTx <- STM.readTChan chan
                    pure $ case eitherBlockTx of
                      Left  blockTx -> Left  (Block.blockTxId blockTx)
                      Right blockTx -> Right (Block.blockTxId blockTx)
              }
          -- N2C: LocalChainSync (full blocks, no BlockFetch needed).
          -----------------------------------------------------------
          NodeToClient socketPath depth -> do
            syncState <- TxIdSyncN2C.emptyState
              TxIdSyncN2C.Config
                { TxIdSyncN2C.confirmationDepth = depth }
            let clients = N2C.emptyClients
                  { N2C.clientChainSync =
                      Just $ TxIdSyncN2C.chainSyncClient syncState
                  }
            pure Runtime.ObserverHandle
              { Runtime.ohRun =
                  Async.withAsync
                    (announceLoop (TxIdSyncN2C.stateBroadcast syncState)) $ \announceAsync -> do
                      -- Link the announce loop so its failure aborts the
                      -- observer instead of being silently swallowed.
                      Async.link announceAsync
                      result <- N2C.connect
                        ioManager codecConfig networkMagic tracers
                        socketPath clients
                      case result of
                        Left err ->
                          die $ "observer " ++ observerName ++ ": " ++ err
                        Right () -> pure ()
              , Runtime.ohSubscribe = do
                  chan <- STM.atomically $
                    STM.dupTChan (TxIdSyncN2C.stateBroadcast syncState)
                  -- Reduce each broadcast BlockTx to its TxId (the recycle key).
                  -- TODO: own-traffic filter, same spot as the N2N adapter
                  -- above (see the note there).
                  pure $ do
                    eitherBlockTx <- STM.readTChan chan
                    pure $ case eitherBlockTx of
                      Left  blockTx -> Left  (Block.blockTxId blockTx)
                      Right blockTx -> Right (Block.blockTxId blockTx)
              }

  -- The 'TargetWorker' callback (the last caller-supplied handler): run once
  -- per 'Target' by 'runWorkload'. Connects to the target node and drives the
  -- TxSubmission2 client with the two fetch actions. Takes the 'IOManager'
  -- first (partial-applied inside 'withIOManager' below), the same shape as
  -- 'mkObserver'.
  let targetWorker ioManager target fetchTx tryFetchTx = do
        addrInfo <- resolveAddr
          (Runtime.targetAddr target)
          (Runtime.targetPort target)
        keepAliveClient <- KeepAlive.keepAliveClient 10
        result <- N2N.connect ioManager codecConfig networkMagic tracers addrInfo
          N2N.emptyClients
            { N2N.clientKeepAlive = Just keepAliveClient
            , N2N.clientTxSubmission = Just $
                TxSubmission.txSubmissionClient
                  (Tracing.trTxSubmission tracers)
                  (Runtime.targetName target)
                  (Runtime.maxBatchSize target)
                  fetchTx tryFetchTx
            }
        case result of
          Left err -> die $ Runtime.targetName target ++ ": " ++ err
          Right () -> pure ()

  -- Start workloads.
  -------------------

  -- IOManager: no-op on POSIX, required on Windows for IOCP. All network I/O
  -- and cleanup must live inside this block as the handle is invalidated when
  -- 'withIOManager' returns.
  withIOManager $ \ioManager -> do
    -- Resolve runtime: creates observers (via mkObserver), pipes, rate
    -- limiters, and spawns builders. All asyncs are linked and tracked.
    runtime <- Runtime.resolve
      mkBuilder
      mkPipeHandle
      mkRecyclerHandle
      (mkObserver ioManager)
      validated
    -- Startup delay.
    -- Sleeps after the builders are already spawned and running so they keep
    -- filling the payload queues for the whole delay, while the workers below
    -- open their connections only after it elapses.
    let startupDelaySeconds = Validated.startupDelaySeconds validated
    when (startupDelaySeconds > 0) $ do
      hPutStrLn stderr $ "Startup delay: waiting " ++ show startupDelaySeconds
        ++ " second(s) (builders pre-filling queues)..."
      threadDelay (fromIntegral startupDelaySeconds * 1_000_000)
      hPutStrLn stderr "Startup delay complete, connecting to targets."
    -- For each 'Workload'.
    workers <- concat <$> mapM
      (\workload -> runWorkload workload (targetWorker ioManager))
      (Map.elems $ Runtime.workloads runtime)
    -- runWorkload returns unlinked asyncs; link them here so failures
    -- propagate to the main thread immediately.
    mapM_ Async.link workers
    -- All asyncs (builders and workers) are linked to the main thread and run
    -- forever. ANY completion, whether by exception or normal return, is fatal:
    -- either the pipeline starved ('QueueStarved'), a connection dropped, or a
    -- builder failed.
    --
    -- 'waitAnyCatch' returns as soon as the first async finishes (without
    -- re-throwing, so we keep control). 'finally cancelAll' then cancels every
    -- remaining async before the program exits.
    --
    -- 'Async.link' is still needed: if the main thread is blocked in
    -- 'waitAnyCatch' waiting on async A but async B dies, 'link' delivers the
    -- exception asynchronously, unblocking 'waitAnyCatch' immediately instead
    -- of waiting for A to finish first.
    let allAsyncs = Runtime.asyncs runtime ++ workers
        cancelAll = mapM_ Async.cancel allAsyncs
    (_, result) <- flip finally cancelAll $
      Async.waitAnyCatch allAsyncs
    case result of
      Left ex ->
        die $ show ex
      Right () ->
        die "async terminated unexpectedly"

--------------------------------------------------------------------------------
-- Input source interpretation.
--------------------------------------------------------------------------------

-- | Interpreted input source: how to obtain UTxO funds. Referenced by
-- @initial_inputs@ (the startup load) and by builder recoveries (rebuilding the
-- queued inputs after a reset).
--
-- This type is node-specific (it references signing keys and sockets), so it
-- lives here rather than in the @pull-fiction@ sub-library, which stores the
-- source params as an opaque 'Aeson.Value' (see 'Raw.InputSource').
data FundSource
  -- | Query the UTxOs at addresses through a NodeToClient socket.
  = UTxOQuerySource !FilePath
  -- | Load funds from a genesis signing-keys file.
  | GenesisKeysSource !FilePath

-- | Interpret a 'Raw.InputSource' (opaque type + params) into a concrete
-- 'FundSource'.
interpretInputSource :: Raw.InputSource -> Either String FundSource
interpretInputSource raw = case Raw.inputSourceType raw of
  "utxo_query" ->
    case Aeson.Types.parseEither parseQuery (Raw.inputSourceParams raw) of
      Left  err -> Left $ "InputSource params error: " ++ err
      Right s   -> Right s
  "genesis_utxo_keys" ->
    case Aeson.Types.parseEither parseGenesis (Raw.inputSourceParams raw) of
      Left  err -> Left $ "InputSource params error: " ++ err
      Right s   -> Right s
  other -> Left $
    "InputSource: unknown \"type\" " ++ show other
    ++ ", expected \"utxo_query\" or \"genesis_utxo_keys\""
  where
    parseQuery = Aeson.withObject "UTxOQuery InputSourceParams" $ \o ->
      UTxOQuerySource <$> o .: "socket_path"
    parseGenesis = Aeson.withObject "GenesisKeys InputSourceParams" $ \o ->
      GenesisKeysSource <$> o .: "signing_keys_file"

-- | Interpret the opaque use-site @params@ of @initial_inputs@ for a
-- @utxo_query@ source: the signing key files. Each key's derived address is
-- queried, and every UTxO found there is tagged with that key so the spending
-- transaction can be signed.
parseInitialKeys :: Aeson.Value -> Aeson.Types.Parser [FilePath]
parseInitialKeys = Aeson.withObject "initial_inputs params" $ \o ->
  o .: "signing_keys"

--------------------------------------------------------------------------------
-- Builder interpretation.
--------------------------------------------------------------------------------

-- | Interpreted "value" builder configuration with defaults applied.
data ValueBuilder
  = ValueBuilder
    { inputsPerTx           :: !Natural
    , outputsPerTx          :: !Natural
    , fee                   :: !Integer
      -- | Non-empty. Output i pays to destination (i mod n), n being the length
      -- of this list, and its recycled fund keeps that key: multiple
      -- destinations let steady-state batches mix keys and stay multi-witness
      -- (typical, not guaranteed; the count follows the keys a batch happens to
      -- draw, see 'TxAssembly.buildTx').
    , destinations :: ![(Api.SigningKey Api.PaymentKey, Api.AddressInEra Era)]
    }

-- | Interpret a 'Raw.Builder' (opaque type + params) into a concrete
-- 'ValueBuilder'. Applies defaults (@inputs_per_tx@ = 1, @outputs_per_tx@ = 1),
-- validates invariants, and resolves the destination signing key and address.
--
-- Each builder pays to (and recycles under) its destination keys: the
-- @destination_signing_keys@ builder param (a list; outputs round-robin over
-- it) or the singular @destination_signing_key@ (setting both is an error),
-- otherwise a per-index built-in key. Addresses are derived from the keys.
interpretBuilder :: Api.NetworkId -> Int -> Raw.Builder -> IO ValueBuilder
interpretBuilder networkId builderIndex raw = case Raw.builderType raw of
  "value" ->
    case Aeson.Types.parseEither parseValueParams (Raw.builderParams raw) of
      Left err -> die $ "Builder params error: " ++ err
      Right (maybeInputs, maybeOutputs, rawFee, maybeDestPath, maybeDestPaths) -> do
        let nInputs  = fromMaybe 1 maybeInputs
            nOutputs = fromMaybe 1 maybeOutputs
        when (nInputs  == 0) $ die "Builder: inputs_per_tx must be >= 1"
        when (nOutputs == 0) $ die "Builder: outputs_per_tx must be >= 1"
        when (rawFee   <  0) $ die "Builder: fee must be >= 0"
        let readDestinations path = do
              eitherSkey <- Fund.readSigningKey path
              case eitherSkey of
                Left err ->
                  die $ "destination signing key (" ++ path ++ "): " ++ err
                Right skey -> pure (skey, Fund.deriveAddress networkId skey)
        dests <- case (maybeDestPath, maybeDestPaths) of
          (Just _, Just _) -> die $
            "Builder: set destination_signing_key or"
            ++ " destination_signing_keys, not both"
          (Nothing, Just []) ->
            die "Builder: destination_signing_keys must be non-empty"
          (Nothing, Just paths) -> mapM readDestinations paths
          (Just path, Nothing)  -> (:[]) <$> readDestinations path
          (Nothing, Nothing)    ->
            pure [createSigningKeyAndAddress networkId builderIndex]
        pure ValueBuilder
          { inputsPerTx  = nInputs
          , outputsPerTx = nOutputs
          , fee          = rawFee
          , destinations = dests
          }
  other -> die $
    "Builder: unknown type " ++ show other ++ ", expected \"value\""
  where
    parseValueParams = Aeson.withObject "ValueParams" $ \o ->
      (,,,,) <$> o .:? "inputs_per_tx"
             <*> o .:? "outputs_per_tx"
             <*> o .:  "fee"
             <*> o .:? "destination_signing_key"
             <*> o .:? "destination_signing_keys"

--------------------------------------------------------------------------------
-- Observer interpretation.
--------------------------------------------------------------------------------

-- | Interpreted observer.
data Observer
  -- | Chain follow via N2N ChainSync (headers) + BlockFetch (blocks).
  = NodeToNode   !String !Int !Natural
  -- | Chain follow via N2C LocalChainSync (full blocks, no BlockFetch needed).
  | NodeToClient !FilePath !Natural

-- | Interpret 'Raw.Observer' (opaque type + params) into a concrete 'Observer'.
interpretObserver :: Raw.Observer -> Either String Observer
interpretObserver raw = case Raw.observerType raw of
  "nodetonode" ->
    case Aeson.Types.parseEither parseN2N (Raw.observerParams raw) of
      Left  err -> Left $ "Observer params error: " ++ err
      Right o   -> Right o
  "nodetoclient" ->
    case Aeson.Types.parseEither parseN2C (Raw.observerParams raw) of
      Left  err -> Left $ "Observer params error: " ++ err
      Right o   -> Right o
  other -> Left $
    "Observer: unknown \"type\" " ++ show other
    ++ ", expected \"nodetonode\" or \"nodetoclient\""
  where
    parseN2N = Aeson.withObject "N2N ObserverParams" $ \o ->
      NodeToNode  <$> o .: "addr"
                  <*> o .: "port"
                  <*> o .: "confirmation_depth"
    parseN2C = Aeson.withObject "N2C ObserverParams" $ \o ->
      NodeToClient <$> o .: "socket_path"
                   <*> o .: "confirmation_depth"

--------------------------------------------------------------------------------
-- Signing key loading
--------------------------------------------------------------------------------

-- | Built-in fallback signing key and address for a builder index, used when a
-- builder has no 'destination_signing_key'. Builds the key from a hex string,
-- applying an integer suffix to the last 3 hex characters, and derives its
-- address via 'deriveAddress'.
createSigningKeyAndAddress
  :: Api.NetworkId
  -> Int
  -> (Api.SigningKey Api.PaymentKey, Api.AddressInEra Era)
createSigningKeyAndAddress networkId n
  | n < 0 || n > 999 =
    error $ "createSigningKeyAndAddress: out of range (0-999): " ++ show n
  | otherwise =
      let -- Hex string (32 bytes = 64 hex chars).
          -- We use 61 chars + 3 chars suffix = 64 chars total.
          -- If the input string is a CBOR-encoded hex string (e.g. from an
          -- .skey file), strip the first 4 characters ("5820") which represent
          -- the CBOR type and length prefix for 32 bytes of raw data.
          prefix = "bed03030fd08a600647d99fa7cd94dae3ddab99b199c3f08f81949db3e422"
          suffix = printf "%03d" n
          hex = prefix ++ suffix
          eitherSkey = Api.deserialiseFromRawBytesHex
                        @(Api.SigningKey Api.PaymentKey)
                        (BS8.pack hex)
      in case eitherSkey of
        Left err -> error $
                      "createSigningKeyAndAddress: Failed to deserialise: "
                      ++ show err
        Right signingKey ->
          (signingKey, Fund.deriveAddress networkId signingKey)

--------------------------------------------------------------------------------
-- Cardano parameters
--------------------------------------------------------------------------------

{-- TODO: Construct a minimal protocol parameters, see TxAssembly.hs last line.
data ProtocolParameters = ProtocolParameters
  { epochLength :: Integer
  , minFeeA     :: Integer
  , minFeeB     :: Integer
  } 

instance Aeson.FromJSON ProtocolParameters where
  parseJSON = Aeson.withObject "ProtocolParameters" $ \o -> do
    pp <- o .: "params"
    ProtocolParameters <$> pp .: "epoch_length" <*> pp .: "min_fee_a" <*> pp .: "min_fee_b"
--}

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Parse CLI args, load all configuration files, create protocol, generate a
-- signing key, load initial funds, and validate config.
--
-- Returns a 'Validated.Config' (validated but not yet resolved into a
-- 'Runtime.Runtime'). The caller is responsible for calling 'Runtime.resolve'
-- to create STM resources.
loadConfig
  :: IO ( -- | Whether this is a dry run: validate, then exit before traffic.
          Bool
          -- | Validated configuration (no STM resources yet).
        , Validated.Config Fund.Fund
          -- | Codec config for serialising blocks on the wire.
        , CodecConfig Block.CardanoBlock
        , Api.NetworkId
          -- | Network magic for the handshake with cardano-node.
        , Api.NetworkMagic
          -- | Logging / metrics tracers.
        , Tracing.Tracers
        )
loadConfig = do
  args <- getArgs
  (isDryRun, configFile) <- case args of
    [cfg]              -> pure (False, cfg)
    ["--dry-run", cfg] -> pure (True, cfg)
    _                  -> die "Usage: tx-centrifuge [--dry-run] <config.json>"

  hPutStrLn stderr "=== Tx Centrifuge ==="
  hPutStrLn stderr ""

  -- Decode the full JSON object once; extract node-specific paths here (like
  -- setupTracers reads trace config from the same file independently) and pass
  -- the rest to the Raw → Validated → Runtime pipeline.
  hPutStrLn stderr $ "Loading config from: " ++ configFile
  rawValue <- Aeson.eitherDecodeFileStrict' configFile
    >>= either (\e -> die $ "JSON: " ++ e) pure
  let parseField field =
        case Aeson.Types.parseEither (Aeson.withObject "Config" (.: field)) rawValue of
          Left err -> die $ "Config: " ++ err
          Right v  -> pure v
  nodeConfigPath <- parseField "nodeConfig"
  raw <- case Aeson.fromJSON rawValue of
    Aeson.Error err   -> die $ "JSON: " ++ err
    Aeson.Success cfg -> pure cfg

  -- Load node configuration and create the consensus protocol first: the
  -- network id it yields is needed to load funds (both to derive query
  -- addresses and to open the LocalStateQuery connection).
  hPutStrLn stderr $ "Loading node config from: " ++ nodeConfigPath
  nodeConfig <- mkNodeConfig nodeConfigPath >>= either die pure
  protocol   <- mkConsensusProtocol nodeConfig >>= either die pure
  codecConfig  <- protocolToCodecConfig protocol
  networkId    <- protocolToNetworkId protocol
  networkMagic <- protocolToNetworkMagic protocol

  -- Load initial funds. Look the initial_inputs source up in the raw config
  -- (validate also checks the reference, but funds are loaded first, so a
  -- missing name dies here), interpret it into the node-level FundSource ADT,
  -- then obtain actual UTxO funds before validation: either from a genesis
  -- signing-keys file, or by querying the node on chain. Each variant announces
  -- itself and returns its loader's result. Handling that result (fail on error
  -- or empty, report the count, build the non-empty list) is shared below.
  funds <- do
    let sourceName  = Raw.initialInputsSource (Raw.initialInputs raw)
        maybeParams = Raw.initialInputsParams (Raw.initialInputs raw)
    rawSource <-
      case Map.lookup sourceName =<< Raw.maybeInputSources raw of
        Nothing  -> die $
          "initial_inputs: undefined input source " ++ show sourceName
        Just src -> pure src
    fundSource <- case interpretInputSource rawSource of
      Left err  -> die $ "input_sources." ++ sourceName ++ ": " ++ err
      Right src -> pure src
    result <- case fundSource of
      GenesisKeysSource path -> do
        -- The source is self-contained, reject use-site params instead of
        -- silently ignoring them.
        case maybeParams of
          Just _  -> die $
            "initial_inputs: a \"genesis_utxo_keys\" source takes no"
            ++ " \"params\""
          Nothing -> pure ()
        hPutStrLn stderr $ "Loading funds from: " ++ path
        Fund.loadFunds networkId path
      UTxOQuerySource socketPath -> do
        keyPaths <- case maybeParams of
          Nothing -> die $
            "initial_inputs: a \"utxo_query\" source needs \"params\" with"
            ++ " the \"signing_keys\" to query under"
          Just params ->
            case Aeson.Types.parseEither parseInitialKeys params of
              Left err    -> die $ "initial_inputs params: " ++ err
              Right paths -> pure paths
        hPutStrLn stderr $ "Querying UTxOs via N2C socket: " ++ socketPath
        Fund.discoverFunds networkId socketPath keyPaths
    case result of
      Left err     -> die $ "initial_inputs: " ++ err
      Right []     -> die "initial_inputs: no funds loaded"
      Right (f:fs) -> do
        let allFunds = f NE.:| fs
        hPutStrLn stderr $ "  Loaded " ++ show (NE.length allFunds) ++ " funds"
        pure allFunds

  -- Validate config.
  -- Pipeline: Raw → Validated (with pre-loaded funds).
  validated <- either die pure $ Validated.validate raw funds

  -- Tracers.
  tracers <- Tracing.setupTracers configFile

  pure ( isDryRun, validated, codecConfig, networkId, networkMagic, tracers )

--------------------------------------------------------------------------------
-- Protocol helpers (inlined from NodeConfig.hs and OuroborosImports.hs)
--------------------------------------------------------------------------------

mkNodeConfig :: FilePath -> IO (Either String NodeConfiguration)
mkNodeConfig configFp_ = do
  configYamlPc <- parseNodeConfigurationFP . Just $ configFp
  pure $ first show $ makeNodeConfiguration (configYamlPc <> filesPc)
  where
    configFp = ConfigYamlFilePath configFp_
    filesPc :: PartialNodeConfiguration
    filesPc = defaultPartialNodeConfiguration
      { pncProtocolFiles = Last . Just $
          ProtocolFilepaths
          { byronCertFile        = Just ""
          , byronKeyFile         = Just ""
          , shelleyKESSource     = Just (KESKeyFilePath "")
          , shelleyVRFFile       = Just ""
          , shelleyBLSFile       = Nothing
          , shelleyCertFile      = Just ""
          , shelleyBulkCredsFile = Just ""
          }
      , pncShutdownConfig = Last $ Just $ ShutdownConfig Nothing Nothing
      , pncConfigFile = Last $ Just configFp
      }

mkConsensusProtocol
  :: NodeConfiguration -> IO (Either String SomeConsensusProtocol)
mkConsensusProtocol nodeConfig =
  case ncProtocolConfig nodeConfig of
    NodeProtocolConfigurationCardano
      byronCfg shelleyCfg alonzoCfg conwayCfg
      dijkstraCfg hardforkCfg checkpointsCfg ->
        first show <$>
          runExceptT (mkSomeConsensusProtocolCardano
            byronCfg shelleyCfg alonzoCfg conwayCfg
            dijkstraCfg hardforkCfg checkpointsCfg Nothing)

-- protocolInfo runs in IO on this branch (block forging setup talks to the
-- KES agent), so these helpers do too.
protocolToCodecConfig :: SomeConsensusProtocol -> IO (CodecConfig Block.CardanoBlock)
protocolToCodecConfig (SomeConsensusProtocol Api.CardanoBlockType info) =
    configCodec . pInfoConfig . fst <$> Api.protocolInfo @IO info
protocolToCodecConfig _ =
  error "protocolToCodecConfig: non-Cardano protocol"

-- | Derive NetworkId from the consensus config. Mainnet uses a
-- well-known magic number; everything else is a testnet.
protocolToNetworkId :: SomeConsensusProtocol -> IO Api.NetworkId
protocolToNetworkId proto = do
  networkMagic <- protocolToNetworkMagic proto
  pure $ case networkMagic of
    Api.NetworkMagic 764824073 -> Api.Mainnet
    nm                         -> Api.Testnet nm

protocolToNetworkMagic :: SomeConsensusProtocol -> IO Api.NetworkMagic
protocolToNetworkMagic
  (SomeConsensusProtocol Api.CardanoBlockType info) =
    getNetworkMagic . configBlock . pInfoConfig . fst <$>
      Api.protocolInfo @IO info
protocolToNetworkMagic _ =
  error "protocolToNetworkMagic: non-Cardano protocol"
