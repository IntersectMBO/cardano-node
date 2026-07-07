{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forM_, unless, when)
import Data.Bifunctor (first)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Last(..))
import Numeric (readHex)
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
----------
-- text --
----------
import Data.Text qualified as T
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
import Cardano.Benchmarking.TxCentrifuge.UTxOQuery qualified as UTxOQuery

--------------------------------------------------------------------------------

main :: IO ()
main = do

  -- Config.
  ----------

  ( validated, codecConfig, networkId, networkMagic, tracers
   , signingKeyPrefix, signingKeyBase, preflight, cooldownSecs
   ) <- loadConfig

  -- Callbacks.
  -------------

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

  -- Observer factory: called by Runtime.resolve for each observer in the
  -- config. Creates an N2N or N2C connection for transaction confirmation
  -- tracking and returns an ObserverHandle. Takes the IOManager as first
  -- argument (partial-applied below).
  let mkObserver ioManager _observerIndex observerName rawObserver = do
        -- From JSON/Aeson.Value to the cardano-node specific observer.
        observer <- case interpretObserver rawObserver of
          Left err -> die $ "Observer " ++ observerName ++ ": " ++ err
          Right o  -> pure o
        case observer of
          -- N2N: ChainSync (headers) + BlockFetch (blocks) + KeepAlive.
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
              { Runtime.ohRun = do
                  result <- N2N.connect
                    ioManager codecConfig networkMagic tracers
                    addrInfo clients
                  case result of
                    Left err ->
                      die $ "observer " ++ observerName ++ ": " ++ err
                    Right () -> pure ()
              , Runtime.ohSubscribe  = STM.atomically $
                  STM.dupTChan (TxIdSyncN2N.stateBroadcast syncState)
              , Runtime.ohExtractKey = Block.blockTxId
              }
          -- N2C: LocalChainSync (full blocks, no BlockFetch needed).
          NodeToClient socketPath depth -> do
            syncState <- TxIdSyncN2C.emptyState
              TxIdSyncN2C.Config
                { TxIdSyncN2C.confirmationDepth = depth }
            let clients = N2C.emptyClients
                  { N2C.clientChainSync =
                      Just $ TxIdSyncN2C.chainSyncClient syncState
                  }
            pure Runtime.ObserverHandle
              { Runtime.ohRun = do
                  result <- N2C.connect
                    ioManager codecConfig networkMagic tracers
                    socketPath clients
                  case result of
                    Left err ->
                      die $ "observer " ++ observerName ++ ": " ++ err
                    Right () -> pure ()
              , Runtime.ohSubscribe  = STM.atomically $
                  STM.dupTChan (TxIdSyncN2C.stateBroadcast syncState)
              , Runtime.ohExtractKey = Block.blockTxId
              }

  -- Builder factory passed to 'Runtime.resolve'. Given a zero-based index,
  -- the builder name, and the opaque builder config, returns a BuilderHandle.
  let mkBuilder builderIndex builderName rawBuilder = do
        -- From JSON/Aeson.Value to the cardano-node specific builder.
        builder <- interpretBuilder rawBuilder
        -- Based on index we create a new unique address/key for each builder.
        let (signingKey, signingAddr) = recycleKeyForWorkload
                                          signingKeyPrefix
                                          signingKeyBase
                                          networkId
                                          builderIndex
        pure Runtime.BuilderHandle
          { -- The number of inputs to wait for.
            Runtime.bhInputsPerBatch = inputsPerTx builder
            -- Build and sign.
          , Runtime.bhBuildPayload   = \inputFunds -> do
              let buildTxAns = TxAssembly.buildTx
                                 signingAddr signingKey
                                 inputFunds (outputsPerTx builder)
                                 (L.Coin (fee builder))
              case buildTxAns of
                Left err -> do
                  -- Drop the inputs instead of dying. Most common cause is
                  -- dust: a UTxO whose value <= fee cannot produce a valid
                  -- (non-zero) change output, so the tx cannot be built.
                  -- Logged here for visibility; the runtime treats Nothing
                  -- as "drop and continue" without recycling these inputs.
                  hPutStrLn stderr $
                    "[" ++ builderName ++ "] dropping inputs: " ++ err
                  forM_ inputFunds $ \f ->
                    hPutStrLn stderr $
                      "  - " ++ show (Fund.fundTxIn f)
                      ++ " (" ++ show (Fund.fundValue f) ++ " lovelace)"
                  pure Nothing
                Right (tx, outputFunds) -> do
                  -- Trace the building action. Suppressed in preflight to
                  -- avoid flooding stderr while the builder runs briefly
                  -- between Runtime.resolve and preflightExit's cancel.
                  unless preflight $
                    Tracing.traceWith
                      (Tracing.trBuilder tracers)
                      (Tracing.mkBuilderNewTx
                        builderName tx inputFunds outputFunds
                      )
                  -- The TxID is needed for the "on_confirm" recycling strategy.
                  let txId = Api.getTxId (Api.getTxBody tx)
                  pure (Just (txId, tx, outputFunds))
          }

  -- IOManager: no-op on POSIX, required on Windows for IOCP. All network I/O
  -- and cleanup must live inside this block as the handle is invalidated when
  -- 'withIOManager' returns.
  withIOManager $ \ioManager -> do
    -- Resolve runtime: creates observers (via mkObserver), pipes, rate
    -- limiters, and spawns builders. All asyncs are linked and tracked.
    runtime <- Runtime.resolve
      mkBuilder
      (mkObserver ioManager)
      (\name txId isOrphan funds ->
        unless preflight $
          Tracing.traceWith
            (Tracing.trBuilder tracers)
            (Tracing.mkBuilderRecycle name txId isOrphan funds)
      )
      (\name stats ->
        unless preflight $
          Tracing.traceWith
            (Tracing.trBuilder tracers)
            (Tracing.mkBuilderQueueDepth name stats)
      )
      validated
    -- Preflight: brief observer connection check + partition summary, then
    -- exit before spawning workers (so no traffic is generated). Builder
    -- asyncs may produce a few payloads before being cancelled — they have
    -- nowhere to send them (no workers).
    when preflight $ preflightExit runtime validated
    -- The 'TargetWorker' callback, called once per 'Target'.
    let targetWorker target fetchTx tryFetchTx = do
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
    -- Cooldown: builders are running and pre-filling payload queues.
    -- Wait for the cluster to stabilise before opening connections so that
    -- transmission begins at the target TPS immediately. Configured via the
    -- top-level @cooldown_seconds@ JSON field; default 0 (no wait).
    when (cooldownSecs > 0) $ do
      hPutStrLn stderr $ "Cooldown: waiting " ++ show cooldownSecs
        ++ " seconds (builders pre-filling queues)..."
      threadDelay (cooldownSecs * 1_000_000)
      hPutStrLn stderr "Cooldown complete, connecting to targets."
    -- For each 'Workload'.
    workers <- concat <$> mapM
      (\workload -> runWorkload workload targetWorker)
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
-- Initial funds.
--------------------------------------------------------------------------------
--
-- Initial UTxOs are discovered on-chain at startup via 'UTxOQuery'. See
-- 'loadInitialFunds' below for the workflow. The legacy @funds.json@-based
-- 'InitialFunds' ADT was removed in favour of stateless on-chain discovery.

--------------------------------------------------------------------------------
-- Builder interpretation.
--------------------------------------------------------------------------------

-- | Interpreted "value" builder configuration with defaults applied.
data ValueBuilder
  = ValueBuilder
    { inputsPerTx  :: !Natural
    , outputsPerTx :: !Natural
    , fee          :: !Integer
    }

-- | Interpret a 'Raw.Builder' (opaque type + params) into a concrete
-- 'ValueBuilder'. Applies defaults (@inputs_per_tx@ = 1, @outputs_per_tx@ = 1)
-- and validates invariants.
interpretBuilder :: Raw.Builder -> IO ValueBuilder
interpretBuilder raw = case Raw.builderType raw of
  "value" ->
    case Aeson.Types.parseEither parseValueParams (Raw.builderParams raw) of
      Left err -> die $ "Builder params error: " ++ err
      Right (maybeInputs, maybeOutputs, rawFee) -> do
        let nInputs  = fromMaybe 1 maybeInputs
            nOutputs = fromMaybe 1 maybeOutputs
        when (nInputs  == 0) $ die "Builder: inputs_per_tx must be >= 1"
        when (nOutputs == 0) $ die "Builder: outputs_per_tx must be >= 1"
        when (rawFee   <  0) $ die "Builder: fee must be >= 0"
        pure ValueBuilder
          { inputsPerTx       = nInputs
          , outputsPerTx      = nOutputs
          , fee               = rawFee
          }
  other -> die $
    "Builder: unknown type " ++ show other ++ ", expected \"value\""
  where
    parseValueParams = Aeson.withObject "ValueParams" $ \o ->
      (,,) <$> o .:? "inputs_per_tx"
           <*> o .:? "outputs_per_tx"
           <*> o .:  "fee"

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

-- | Load the operator-supplied recycle signing key and return the 61-char
-- hex prefix plus the workload-0 base suffix (as an integer in @0..4095@).
--
-- Reads a standard cardano-cli text-envelope @.skey@ file (payment or genesis
-- UTxO key type), hex-encodes the 32-byte raw secret, and splits it into a
-- 61-char prefix + 3-char hex suffix. Any 3-hex-char suffix is accepted; the
-- supplied key always corresponds to workload 0, and workload N's recycle
-- key uses suffix @(base + N) mod 0x1000@.
--
-- This means cardano-cli-generated keys can be used as-is: the operator does
-- not need to engineer a particular suffix. Discovery scans all 4096 possible
-- addresses regardless of base, so previously-funded UTxOs at any suffix are
-- picked up automatically.
loadSigningKeyPrefix :: FilePath -> IO (String, Int)
loadSigningKeyPrefix path = do
  eKey <- Fund.readSigningKey path
  skey <- case eKey of
    Left  err -> die $ "signing_key_file " ++ show path ++ ": " ++ err
    Right k   -> pure k
  let hex = BS8.unpack (Api.serialiseToRawBytesHex skey)
  when (length hex /= 64) $
    die $ "signing_key_file " ++ show path
       ++ ": expected 64 hex chars (32-byte ed25519 key), got "
       ++ show (length hex)
  let (prefix, suffix) = splitAt 61 hex
  base <- case readHex suffix of
    [(n, "")] | n >= 0 && n <= 0xfff -> pure n
    _ -> die $ "signing_key_file " ++ show path
            ++ ": trailing 3 hex chars not a valid hex value: "
            ++ show suffix
  hPutStrLn stderr $
    "  Workload 0 base suffix: '" ++ suffix
    ++ "' (index " ++ show base ++ ")"
  pure (prefix, base)

--------------------------------------------------------------------------------
-- Initial UTxO discovery
--------------------------------------------------------------------------------

-- | Discover the live set of UTxOs spendable by this tx-centrifuge instance.
--
-- Derives all 1000 @(skey, addr)@ pairs from the operator's seed prefix
-- (indices @000..999@), then queries the local node for UTxOs at every one of
-- those addresses in a single @QueryUTxOByAddress@ call. UTxOs found at index
-- @n@'s address are tagged with index @n@'s signing key, so they can later be
-- spent regardless of which workload's input queue they land in.
--
-- The query socket is the @socket_path@ of the first declared @nodetoclient@
-- observer (alphabetical by observer name). At least one such observer is
-- required; tx-centrifuge dies clearly if none is configured. This intentional
-- coupling lets tx-centrifuge rely on the operator's existing N2C node connection
-- rather than introducing a separate top-level socket field.
--
-- Dies if the *total* across all scanned addresses is zero, printing
-- workload 0's bech32 address so the operator knows where to send the
-- funding transaction.
discoverInitialFunds
  :: Raw.Config
  -> String          -- ^ 61-char signing-key prefix.
  -> Int             -- ^ Workload-0 base suffix (0..4095).
  -> Api.NetworkId
  -> IO (NE.NonEmpty Fund.Fund)
discoverInitialFunds raw signingKeyPrefix signingKeyBase networkId = do
  -- Derive every (skey, addr) pair in the 3-hex-char suffix space (4096
  -- entries). Cheap: just ed25519 derivations.
  let scanEntries =
        [ (i, keyForSuffixIndex signingKeyPrefix networkId i)
        | i <- [0 .. suffixSpace - 1]
        ]
      scanAddrs = map (\(_, (_, addr)) -> addr) scanEntries
      -- Workload 0's address (the supplied skey's address — what the
      -- operator should fund if starting fresh).
      workload0Addr = snd (keyForSuffixIndex signingKeyPrefix networkId
                            signingKeyBase)

  -- Find the first 'nodetoclient' observer's socket path. By Map order
  -- (alphabetical by observer name) — document this in the README.
  queryObsSocket <- findFirstNodeToClientSocket raw

  hPutStrLn stderr $
    "Querying UTxOs via N2C socket: " ++ queryObsSocket
  utxosByAddr <- UTxOQuery.queryUTxOsAtAddresses
    queryObsSocket networkId scanAddrs
    >>= either (\e -> die $ "UTxO query failed: " ++ e) pure

  -- Compute the dust threshold: a UTxO with value <= maxFee cannot produce
  -- a valid non-zero change output for the most expensive configured
  -- builder, so it is unspendable by at least one workload. Filter such
  -- UTxOs out at discovery time so they never enter the input queue.
  let dustThreshold = maxConfiguredFee raw

  -- Build per-address (spendable, dust) split. The query returns results
  -- keyed by 'AddressAny' (era-agnostic), so convert our ConwayEra-typed
  -- addresses for lookup.
  let toAddrSummary (i, (skey, addr)) =
        case Map.lookup (addressInEraToAny addr) utxosByAddr of
          Nothing    -> Nothing
          Just utxos ->
            let (spendableU, dustU) = partition
                  (\(_, val) -> val > dustThreshold) utxos
                spendableFunds = map
                  (\(txin, val) -> Fund.Fund
                    { Fund.fundTxIn    = txin
                    , Fund.fundValue   = val
                    , Fund.fundSignKey = skey
                    })
                  spendableU
            in Just (i, addr, spendableFunds, length dustU)
      addrSummaries = mapMaybe toAddrSummary scanEntries
      allFunds      = concatMap (\(_, _, fs, _) -> fs) addrSummaries
      totalSpend    = length allFunds
      totalDust     = sum (map (\(_, _, _, d) -> d) addrSummaries)

  case allFunds of
    [] -> die $ unlines $
      [ "No spendable UTxOs found at any of the "
        ++ show suffixSpace ++ " scanned recycle addresses."
      ]
      ++ (if totalDust > 0
            then [ "(" ++ show totalDust
                   ++ " dust UTxO(s) at or below the configured fee of "
                   ++ show dustThreshold ++ " lovelace were skipped.)" ]
            else [])
      ++
      [ "Fund workload 0's address:"
      , "  " ++ T.unpack (Api.serialiseAddress workload0Addr)
      , "e.g.:"
      , "  cardano-cli " ++ eraSubcommand ++ " transaction build \\"
      , "    --tx-out <addr-above>+<lovelace> \\"
      , "    ..."
      , "Then restart tx-centrifuge."
      ]
    (f:fs) -> do
      hPutStrLn stderr $
        "Discovered UTxOs (dust threshold = "
        ++ show dustThreshold ++ " lovelace):"
      forM_ addrSummaries $ \(i, addr, fs', dustCount) ->
        let count  = length fs'
            totAda = sum (map Fund.fundValue fs') `div` 1_000_000
        in hPutStrLn stderr $
             printf "  %03x  %s  %d UTxOs, %d ADA  (%d dust skipped)"
               i (T.unpack (Api.serialiseAddress addr))
               count totAda dustCount
      hPutStrLn stderr $
        "Total: " ++ show totalSpend
        ++ " spendable UTxOs across " ++ show (length addrSummaries)
        ++ " address(es); " ++ show totalDust ++ " dust skipped"
      pure (f NE.:| fs)
  where
    -- Match the era used everywhere else in tx-centrifuge.
    eraSubcommand = "conway"

-- | Erase the era index of an 'AddressInEra' for matching against the
-- 'AddressAny' keys returned by 'UTxOQuery.queryUTxOsAtAddresses'. Address
-- bytes are identical across Shelley-based eras; the era index is purely
-- type-level.
addressInEraToAny :: Api.AddressInEra era -> Api.AddressAny
addressInEraToAny (Api.AddressInEra _ addr) = Api.toAddressAny addr

-- | Maximum @fee@ across every configured builder (top-level + per-workload).
-- This is the conservative dust threshold: a UTxO at or below this value
-- cannot produce a valid change output for *any* configured workload.
-- Defaults to 1 ADA (1_000_000 lovelace) if no builder declares a fee. This
-- matches the codebase's convention of integer-ADA fees in the examples
-- (real-network minimums for a simple value tx are ~0.2 ADA; 1 ADA rounds
-- up cleanly and keeps recycled UTxO values on integer-ADA boundaries).
maxConfiguredFee :: Raw.Config -> Integer
maxConfiguredFee raw = case configuredFees of
  [] -> 1_000_000
  fs -> maximum fs
  where
    topBuilders = maybe [] (:[]) (Raw.maybeTopLevelBuilder raw)
    wlBuilders  = case Raw.maybeWorkloads raw of
      Nothing -> []
      Just wls -> mapMaybe Raw.maybeBuilder (Map.elems wls)
    configuredFees =
      mapMaybe
        (\b -> Aeson.Types.parseMaybe
                 (Aeson.withObject "ValueParams" (.: "fee"))
                 (Raw.builderParams b))
        (topBuilders ++ wlBuilders)

-- | Locate the @socket_path@ of the first @nodetoclient@ observer in the
-- raw config. Picks alphabetically by observer name when multiple are
-- declared (see README). Dies with a helpful message if none is configured.
findFirstNodeToClientSocket :: Raw.Config -> IO FilePath
findFirstNodeToClientSocket raw = case Raw.maybeObservers raw of
  Nothing -> die noN2CMsg
  Just obsMap ->
    let entries = Map.toAscList obsMap
        pick (name, obs) = case Raw.observerType obs of
          "nodetoclient" ->
            case Aeson.Types.parseEither
                   (Aeson.withObject "N2C ObserverParams" (.: "socket_path"))
                   (Raw.observerParams obs) of
              Right path -> Just (name, path :: FilePath)
              Left  _    -> Nothing
          _ -> Nothing
    in case mapMaybe pick entries of
      []                  -> die noN2CMsg
      ((name, path) : _ ) -> do
        hPutStrLn stderr $
          "Using N2C observer '" ++ name
          ++ "' for initial UTxO discovery"
        pure path
  where
    noN2CMsg = unlines
      [ "tx-centrifuge requires at least one 'nodetoclient' observer for"
      , "initial UTxO discovery. Add one to your config, e.g.:"
      , "  \"observers\": {"
      , "    \"local-follower\": {"
      , "      \"type\": \"nodetoclient\","
      , "      \"params\": {"
      , "        \"socket_path\": \"/run/cardano-node/node.socket\","
      , "        \"confirmation_depth\": 2"
      , "      }"
      , "    }"
      , "  }"
      ]

--------------------------------------------------------------------------------
-- Builder address derivation
--------------------------------------------------------------------------------

-- | Number of distinct keys/addresses derivable from the prefix. 3 hex
-- chars → 4096 unique suffixes.
suffixSpace :: Int
suffixSpace = 0x1000

-- | Derive the signing key + address for the address-space index @i@
-- (@0..4095@), using the prefix + a 3-hex-char suffix. This is the
-- address-level derivation; see 'recycleKeyForWorkload' for the
-- workload-index mapping.
keyForSuffixIndex
  :: String
  -- ^ 61-char hex prefix from 'loadSigningKeyPrefix'.
  -> Api.NetworkId
  -> Int
  -- ^ Suffix index (@0..4095@, encoded as 3-hex-char suffix).
  -> (Api.SigningKey Api.PaymentKey, Api.AddressInEra Api.ConwayEra)
keyForSuffixIndex prefix networkId i
  | i < 0 || i >= suffixSpace =
    error $ "keyForSuffixIndex: out of range (0-"
         ++ show (suffixSpace - 1) ++ "): " ++ show i
  | otherwise =
      let suffix = printf "%03x" i
          hex = prefix ++ suffix
          eitherSkey = Api.deserialiseFromRawBytesHex
                        @(Api.SigningKey Api.PaymentKey)
                        (BS8.pack hex)
      in case eitherSkey of
        Left err -> error $
                      "keyForSuffixIndex: Failed to deserialise: "
                      ++ show err
        Right signingKey ->
          let signingAddr =
                Api.shelleyAddressInEra
                  (Api.shelleyBasedEra @Api.ConwayEra) $
                Api.makeShelleyAddress networkId
                  (Api.PaymentCredentialByKey
                    (Api.verificationKeyHash
                      (Api.getVerificationKey signingKey)))
                  Api.NoStakeAddress
          in (signingKey, signingAddr)

-- | Recycle key + address for workload index @n@, given the workload-0 base
-- suffix from the supplied skey. Workload 0 reproduces the supplied key
-- exactly; workload N uses suffix @(base + n) mod 4096@.
recycleKeyForWorkload
  :: String
  -> Int
  -- ^ Workload-0 base suffix (from 'loadSigningKeyPrefix').
  -> Api.NetworkId
  -> Int
  -- ^ Workload index.
  -> (Api.SigningKey Api.PaymentKey, Api.AddressInEra Api.ConwayEra)
recycleKeyForWorkload prefix base networkId n =
  keyForSuffixIndex prefix networkId ((base + n) `mod` suffixSpace)

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
-- Preflight
--------------------------------------------------------------------------------

-- | Brief observer connection check + per-workload partition summary, then
-- 'exitSuccess'. Cancels every async spawned by 'Runtime.resolve' (observers,
-- builders, recyclers) so the process exits cleanly without leaking threads.
--
-- The 2-second observer settle window is intentionally short: long enough for
-- a healthy local N2C ChainSync handshake to complete, short enough that an
-- ops health check is cheap.
preflightExit
  :: Runtime.Runtime Fund.Fund payload
  -> Validated.Config Fund.Fund
  -> IO a
preflightExit runtime validated = do
  hPutStrLn stderr ""
  hPutStrLn stderr "=== Preflight ==="

  -- Give observers a brief moment to connect; then check none died.
  let observerCount = length (Runtime.observers runtime)
  when (observerCount > 0) $ do
    hPutStrLn stderr $
      "Waiting for " ++ show observerCount
      ++ " observer connection(s) to settle..."
    -- 'Runtime.resolve' links observer asyncs to the main thread, so a
    -- connection failure during the wait surfaces here as a linked-thread
    -- exception. Catch it explicitly to produce a preflight-shaped error
    -- message instead of an unhandled crash.
    threadDelay 2_000_000 `catch` \(ex :: SomeException) ->
      die $ "Preflight: observer failed during settle: " ++ show ex
    forM_ (Runtime.observers runtime) $ \obs -> do
      status <- Async.poll (Runtime.observerAsync obs)
      case status of
        Just (Left ex) ->
          die $ "Observer '" ++ Runtime.observerName obs
             ++ "' failed during preflight: " ++ show ex
        Just (Right ()) ->
          die $ "Observer '" ++ Runtime.observerName obs
             ++ "' exited unexpectedly during preflight"
        Nothing -> pure ()
    hPutStrLn stderr "Observer(s) connected OK"

  -- Per-workload partition summary. Mirrors what 'Runtime.resolve' actually
  -- did: a round-robin chunking via 'partitionInputs'.
  let workloadNames = Map.keys (Validated.workloads validated)
      workloadCount = length workloadNames
      allFunds      = NE.toList (Validated.initialInputs validated)
      chunks        = Runtime.partitionInputs workloadCount allFunds
  hPutStrLn stderr ""
  hPutStrLn stderr $
    "Partitioning " ++ show (length allFunds)
    ++ " UTxOs across " ++ show workloadCount ++ " workload(s):"
  forM_ (zip workloadNames chunks) $ \(name, chunk) ->
    let count = length chunk
        ada   = sum (map Fund.fundValue chunk) `div` 1_000_000
    in hPutStrLn stderr $
         printf "  %-20s  %d UTxOs, %d ADA" name count ada

  hPutStrLn stderr ""
  hPutStrLn stderr "Preflight OK"
  mapM_ Async.cancel (Runtime.asyncs runtime)
  exitSuccess

--------------------------------------------------------------------------------
-- CLI argument parsing
--------------------------------------------------------------------------------

-- | Accept either:
--
-- > tx-centrifuge <config.json>
-- > tx-centrifuge <config.json> --preflight
-- > tx-centrifuge --preflight <config.json>
--
-- In preflight mode, tx-centrifuge runs all initialization (config parsing,
-- protocol setup, signing-key load, UTxO discovery, runtime resolution,
-- observer connection check, partition summary) and then exits @0@ before
-- any worker threads connect to target nodes. Useful for verifying ops
-- changes (new skey, fresh funding, node-config tweaks) without producing
-- traffic.
parseArgs :: [String] -> Maybe (FilePath, Bool)
parseArgs = go False []
  where
    go pf positional [] = case positional of
      [f] -> Just (f, pf)
      _   -> Nothing
    go _  positional ("--preflight" : rest) = go True positional rest
    go pf positional (x            : rest) = go pf (positional ++ [x]) rest

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
  :: IO ( -- | Validated configuration (no STM resources yet).
          Validated.Config Fund.Fund
          -- | Codec config for serialising blocks on the wire.
        , CodecConfig Block.CardanoBlock
        , Api.NetworkId
          -- | Network magic for the handshake with cardano-node.
        , Api.NetworkMagic
          -- | Logging / metrics tracers.
        , Tracing.Tracers
          -- | 61-char hex prefix derived from the operator-supplied recycle
          -- signing key. Workload N's suffix is @(base + N) mod 4096@.
        , String
          -- | Workload-0 base suffix (the supplied key's last 3 hex chars,
          -- parsed as a hex integer in @0..4095@).
        , Int
          -- | Preflight mode flag (from @--preflight@ CLI arg).
        , Bool
          -- | Cooldown seconds before workers connect (top-level config
          -- field @cooldown_seconds@; default 0).
        , Int
        )
loadConfig = do
  args <- getArgs
  (configFile, preflight) <- case parseArgs args of
    Just r  -> pure r
    Nothing -> die "Usage: tx-centrifuge <config.json> [--preflight]"

  hPutStrLn stderr "=== Tx Centrifuge ==="
  when preflight $ hPutStrLn stderr "(preflight mode)"
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
      parseOptField field def =
        case Aeson.Types.parseEither (Aeson.withObject "Config" (.:? field)) rawValue of
          Left err -> die $ "Config: " ++ err
          Right mv -> pure (fromMaybe def mv)
  nodeConfigPath  <- parseField "nodeConfig"
  signingKeyPath  <- parseField "signing_key_file"
  cooldownSecs    <- parseOptField "cooldown_seconds" (0 :: Int)
  raw <- case Aeson.fromJSON rawValue of
    Aeson.Error err   -> die $ "JSON: " ++ err
    Aeson.Success cfg -> pure cfg

  -- Load node configuration and create consensus protocol.
  -- NetworkId/NetworkMagic are derived here; they are needed before the UTxO
  -- query (for both address derivation and the LocalStateQuery connection).
  hPutStrLn stderr $ "Loading node config from: " ++ nodeConfigPath
  nodeConfig <- mkNodeConfig nodeConfigPath >>= either die pure
  protocol   <- mkConsensusProtocol nodeConfig >>= either die pure
  codecConfig  <- protocolToCodecConfig protocol
  networkId    <- protocolToNetworkId protocol
  networkMagic <- protocolToNetworkMagic protocol

  -- Load operator-supplied recycle signing key.
  hPutStrLn stderr $ "Loading signing key from: " ++ signingKeyPath
  (signingKeyPrefix, signingKeyBase) <- loadSigningKeyPrefix signingKeyPath

  -- Discover initial UTxOs on-chain.
  -- Stateless restart: the live fund set is whatever UTxOs sit at our
  -- recycle addresses right now, queried via N2C LocalStateQuery. The
  -- legacy funds.json/initial_inputs pathway no longer exists.
  funds <- discoverInitialFunds raw signingKeyPrefix signingKeyBase networkId

  -- Validate config (pull-fiction validation; funds were obtained out-of-band).
  validated <- either die pure $ Validated.validate raw funds

  -- Tracers.
  tracers <- Tracing.setupTracers configFile

  pure ( validated, codecConfig, networkId, networkMagic, tracers
       , signingKeyPrefix, signingKeyBase, preflight, cooldownSecs
       )

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
    getNetworkMagic . configBlock . pInfoConfig . fst
      <$> Api.protocolInfo @IO info
protocolToNetworkMagic _ =
  error "protocolToNetworkMagic: non-Cardano protocol"

