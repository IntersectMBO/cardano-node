{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Exception (finally)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import System.Exit (die)
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

main :: IO ()
main = do

  -- Config.
  ----------

  (validated, codecConfig, networkId, networkMagic, tracers) <- loadConfig

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
              , Runtime.ohExtractKey = Block.confirmedTxId
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
              , Runtime.ohExtractKey = Block.confirmedTxId
              }

  -- Builder factory passed to 'Runtime.resolve'. Given a zero-based index,
  -- the builder name, and the opaque builder config, returns a BuilderHandle.
  let mkBuilder builderIndex builderName rawBuilder = do
        -- From JSON/Aeson.Value to the cardano-node specific builder.
        builder <- interpretBuilder rawBuilder
        -- Based on index we create a new unique address/key for each builder.
        let (signingKey, signingAddr) = createSigningKeyAndAddress
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
                Left err -> die $ "TxAssembly.buildTx: " ++ err
                Right (tx, outputFunds) -> do
                  -- Trace the building action.
                  Tracing.traceWith
                    (Tracing.trBuilder tracers)
                    (Tracing.mkBuilderNewTx
                      builderName tx inputFunds outputFunds
                    )
                  -- The TxID is needed for the "on_confirm" recycling strategy.
                  let txId = Api.getTxId (Api.getTxBody tx)
                  pure (txId, tx, outputFunds)
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
      (\name funds ->
        Tracing.traceWith
          (Tracing.trBuilder tracers)
          (Tracing.mkBuilderRecycle name funds)
      )
      validated
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

-- | How to load initial funds for the generator.
--
-- This type is node-specific (it references signing keys and network magic),
-- so it lives here rather than in the @pull-fiction@ sub-library. The raw JSON
-- config stores this as an opaque 'Aeson.Value'; @Main@ parses it into this ADT
-- and loads funds before passing them to 'Validated.validate'.
data InitialFunds
  = GenesisUTxOKeys
      !Natural  -- ^ Network magic.
      !FilePath -- ^ Path to signing keys file.

instance Aeson.FromJSON InitialFunds where
  parseJSON = Aeson.withObject "InitialFunds" $ \o -> do
    ty <- o .: "type" :: Aeson.Types.Parser String
    case ty of
      "genesis_utxo_keys" -> do
        p <- o .: "params"
        GenesisUTxOKeys <$> p .: "network_magic" <*> p .: "signing_keys_file"
      _ -> fail $ "InitialFunds: unknown type " ++ show ty
                ++ ", expected \"genesis_utxo_keys\""

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

-- | Load a signing key from a hex string, applying an integer suffix to the
-- last 3 hex characters, and derive its address.
createSigningKeyAndAddress
  :: Api.NetworkId
  -> Int
  -- Signing key used for all generated transactions.
  -- Destination address derived from the signing key.
  -> (Api.SigningKey Api.PaymentKey, Api.AddressInEra Api.ConwayEra)
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
          let signingAddr =
                Api.shelleyAddressInEra
                  (Api.shelleyBasedEra @Api.ConwayEra) $
                Api.makeShelleyAddress networkId
                  (Api.PaymentCredentialByKey
                    (Api.verificationKeyHash
                      (Api.getVerificationKey signingKey)))
                  Api.NoStakeAddress
          in (signingKey, signingAddr)

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
  :: IO ( -- | Validated configuration (no STM resources yet).
          Validated.Config Fund.Fund
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
  configFile <- case args of
    [f] -> pure f
    _   -> die "Usage: tx-centrifuge <config.json>"

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

  -- Load initial funds.
  -- Parse the opaque initialInputs JSON into the node-level InitialFunds ADT,
  -- then load actual UTxO funds before validation.
  funds <- case Aeson.fromJSON (Raw.initialInputs raw) of
    Aeson.Error err -> die $ "initialInputs: " ++ err
    Aeson.Success (GenesisUTxOKeys magic path) -> do
      hPutStrLn stderr $ "Loading funds from: " ++ path
      result <- Fund.loadFunds (magicToNetworkId magic) path
      case result of
        Left err -> die ("Fund.loadFunds: " ++ err)
        Right [] -> die "Fund.loadFunds: no funds loaded"
        Right (f:fs) -> do
          let allFunds = f NE.:| fs
          hPutStrLn stderr $ "  Loaded " ++ show (NE.length allFunds) ++ " funds"
          pure allFunds
  -- Validate config.
  -- Pipeline: Raw → Validated (with pre-loaded funds).
  validated <- either die pure $ Validated.validate raw funds

  -- Load node configuration and create consensus protocol.
  hPutStrLn stderr $ "Loading node config from: " ++ nodeConfigPath
  nodeConfig <- mkNodeConfig nodeConfigPath >>= either die pure
  protocol   <- mkConsensusProtocol nodeConfig >>= either die pure
  let codecConfig  = protocolToCodecConfig protocol
      networkId    = protocolToNetworkId protocol
      networkMagic = protocolToNetworkMagic protocol

  -- Tracers.
  tracers <- Tracing.setupTracers configFile

  pure ( validated, codecConfig, networkId, networkMagic, tracers )

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

protocolToCodecConfig :: SomeConsensusProtocol -> CodecConfig Block.CardanoBlock
protocolToCodecConfig (SomeConsensusProtocol Api.CardanoBlockType info) =
    configCodec $ pInfoConfig $ fst $ Api.protocolInfo @IO info
protocolToCodecConfig _ =
  error "protocolToCodecConfig: non-Cardano protocol"

-- | Derive NetworkId from the consensus config. Mainnet uses a
-- well-known magic number; everything else is a testnet.
protocolToNetworkId :: SomeConsensusProtocol -> Api.NetworkId
protocolToNetworkId proto = case protocolToNetworkMagic proto of
  Api.NetworkMagic 764824073 -> Api.Mainnet
  nm                         -> Api.Testnet nm

protocolToNetworkMagic :: SomeConsensusProtocol -> Api.NetworkMagic
protocolToNetworkMagic
  (SomeConsensusProtocol Api.CardanoBlockType info) =
    getNetworkMagic $ configBlock $ pInfoConfig $
      fst $ Api.protocolInfo @IO info
protocolToNetworkMagic _ =
  error "protocolToNetworkMagic: non-Cardano protocol"

-- | Convert a raw network magic number to a 'Api.NetworkId'.
-- Mainnet uses the well-known magic 764824073; everything else is a testnet.
magicToNetworkId :: Natural -> Api.NetworkId
magicToNetworkId 764824073 = Api.Mainnet
magicToNetworkId n         = Api.Testnet (Api.NetworkMagic (fromIntegral n))
