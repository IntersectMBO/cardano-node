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
import Control.Concurrent (myThreadId)
import Control.Exception (finally)
import Control.Monad (forever, replicateM, when)
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import GHC.Conc (labelThread)
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
--------------------------
-- ouroboros-consensus --
--------------------------
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Config (configBlock, configCodec)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..))
---------------------------------
-- ouroboros-network-framework --
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
---------------------
-- tx-centrifuge --
---------------------
import Cardano.Benchmarking.TxCentrifuge.Client (mkClient)
import Cardano.Benchmarking.TxCentrifuge.Connection
  (CardanoBlock, connect)
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
import Cardano.Benchmarking.TxCentrifuge.Tracing qualified as Tracing
import Cardano.Benchmarking.TxCentrifuge.Tx qualified as Tx

--------------------------------------------------------------------------------

main :: IO ()
main = do

  -- Config.
  ----------

  (runtime, codecConfig, networkId, networkMagic, tracers) <- loadConfig

  -- Launch.
  ----------

  -- Start tx builders Asyncs.
  -- A tx builder sits between the fund queue and the tx queue:
  -- it pulls unspent funds, signs a transaction, and enqueues the
  -- (tx, outputFunds) pair for a worker to submit.
  -- The fund queue is unbounded (TQueue); the tx queue is bounded (TBQueue)
  -- and provides backpressure.
  let mkBuilder runtimeBuilder builderIndex = do
        vb <- interpretBuilder (Runtime.parsedBuilder runtimeBuilder)
        -- Create a "dEtERmiNisTic" signing key and its derived address.
        -- All tx outputs go to this address; recycled funds carry this key.
        let (signingKey, signingAddr) = createSigningKeyAndAddress networkId builderIndex
        let builderName = Runtime.builderName runtimeBuilder
            builderPipe = Runtime.builderPipe runtimeBuilder
            fundQueue   = Runtime.pipeInputQueue builderPipe
            txQueue     = Runtime.pipePayloadQueue builderPipe
            builderLoop = forever $ do
              inputFunds <- STM.atomically $
                replicateM
                  (fromIntegral (inputsPerTx vb))
                  (STM.readTQueue fundQueue)
              case Tx.buildTx signingAddr signingKey
                           inputFunds (outputsPerTx vb) (L.Coin (fee vb)) of
                Left err -> die $ "Tx.buildTx: " ++ err
                Right ans@(tx, outputFunds) -> do
                  Tracing.traceWith
                    (Tracing.trBuilder tracers)
                    (Tracing.mkBuilderNewTx builderName tx inputFunds outputFunds)
                  case optimisticRecycle vb of
                    False -> do
                      STM.atomically $ STM.writeTBQueue txQueue ans
                    True -> do
                      STM.atomically $ STM.writeTBQueue txQueue (tx,[])
                      STM.atomically $ Runtime.pipeRecycle builderPipe outputFunds
                      Tracing.traceWith
                        (Tracing.trBuilder tracers)
                        (Tracing.mkBuilderRecycle builderName outputFunds)
        async <- Async.async $ do
          -- Always label the threads.
          tid <- myThreadId
          labelThread tid (Runtime.builderName runtimeBuilder)
          builderLoop
        -- Return linked async and with a labeled thread.
        Async.link async
        pure async

  -- IOManager (from ouroboros-network-framework, re-exported from
  -- Win32-network) is a platform abstraction for asynchronous I/O:
  --
  --   * On Windows it wraps an I/O Completion Port (IOCP) and spawns a
  --     dedicated OS thread to dequeue completion packets. Sockets are
  --     associated with this IOCP via 'associateWithIOManager'.
  --
  --   * On POSIX (Linux, macOS) it is a complete no-op — the type is
  --     @newtype IOManager = IOManager (forall hole. hole -> IO ())@ and
  --     'withIOManager' simply passes a dummy value to the callback. GHC's
  --     built-in RTS I/O manager (epoll / kqueue) handles all socket
  --     multiplexing transparently.
  --
  -- The Win32-network documentation states that only one IOManager should run
  -- at a time, so we cannot create one per target for isolation. Target
  -- isolation comes from each target having its own Async thread and TCP
  -- socket, not from the IOManager.
  --
  -- Everything must live inside 'withIOManager' because on Windows the IOCP
  -- handle is closed when 'withIOManager' returns, silently cancelling all
  -- pending socket I/O. The 'finally cancelAll' block must therefore be
  -- inside too — putting it outside would try to clean up asyncs whose network
  -- I/O is already dead.
  --
  -- Builders don't use ioManager (they only do STM queue operations and
  -- 'buildTx'), but they are spawned inside so that the 'finally cancelAll'
  -- cleanup covers them. Spawning them outside would leak asyncs if any
  -- setup code between here and 'cancelAll' throws.
  withIOManager $ \ioManager -> do
    -- Start builders provinding a numeric index (zero based).
    builders <- mapM
                  (uncurry mkBuilder)
                  (zip
                    (Runtime.builders runtime)
                    [0..]
                  )
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
            []    -> die $
              "Cannot resolve target: " ++ ip
              ++ ":" ++ show port
            (a:_) -> pure a
    -- The 'TargetWorker' callback, called once per 'Target'.
    let targetWorker target fetchTx tryFetchTx = do
          addrInfo <- resolveAddr
            (Runtime.targetAddr target)
            (Runtime.targetPort target)
          result <- connect ioManager codecConfig networkMagic tracers addrInfo $
            mkClient
              (Tracing.trTxSubmission tracers)
              (Runtime.targetName target)
              (Runtime.maxBatchSize target)
              fetchTx tryFetchTx
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
    -- forever. ANY completion — whether by exception or normal return — is
    -- fatal: either the pipeline starved ('QueueStarved'), a connection
    -- dropped, or a builder failed.
    --
    -- 'waitAnyCatch' returns as soon as the first async finishes (without
    -- re-throwing, so we keep control). 'finally cancelAll' then cancels every
    -- remaining async before the program exits.
    --
    -- 'Async.link' is still needed: if the main thread is blocked in
    -- 'waitAnyCatch' waiting on async A but async B dies, 'link' delivers the
    -- exception asynchronously, unblocking 'waitAnyCatch' immediately instead
    -- of waiting for A to finish first.
    let allAsyncs = builders ++ workers
        cancelAll = mapM_ Async.cancel allAsyncs
    (_, result) <- flip finally cancelAll $
      Async.waitAnyCatch allAsyncs
    case result of
      Left ex ->
        die $ show ex
      Right () ->
        die "async terminated unexpectedly"

--------------------------------------------------------------------------------
-- Initial funds
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
-- Builder interpretation
--------------------------------------------------------------------------------

-- | Interpreted "value" builder configuration with defaults applied.
data ValueBuilder = ValueBuilder
  { inputsPerTx       :: !Natural
  , outputsPerTx      :: !Natural
  , fee               :: !Integer
  , optimisticRecycle :: !Bool
  }

-- | Interpret a 'Raw.Builder' (opaque type + params) into a concrete
-- 'ValueBuilder'. Applies defaults (@inputs_per_tx@ = 1, @outputs_per_tx@ = 1)
-- and validates invariants.
interpretBuilder :: Raw.Builder -> IO ValueBuilder
interpretBuilder raw = case Raw.builderType raw of
  "value" ->
    case Aeson.Types.parseEither parseValueParams (Raw.builderParams raw) of
      Left err -> die $ "Builder params: " ++ err
      Right (maybeInputs, maybeOutputs, rawFee, mOR) -> do
        let nInputs  = fromMaybe 1 maybeInputs
            nOutputs = fromMaybe 1 maybeOutputs
        when (nInputs == 0) $ die "Builder: inputs_per_tx must be >= 1"
        when (nOutputs == 0) $ die "Builder: outputs_per_tx must be >= 1"
        when (rawFee < 0) $ die "Builder: fee must be >= 0"
        pure ValueBuilder
          { inputsPerTx        = nInputs
          , outputsPerTx       = nOutputs
          , fee                = rawFee
          , optimisticRecycle = case mOR of
                                  Nothing -> False
                                  Just oR -> oR
          }
  other -> die $
    "Builder: unknown type " ++ show other ++ ", expected \"value\""
  where
    parseValueParams = Aeson.withObject "ValueParams" $ \o ->
      (,,,) <$> o .:? "inputs_per_tx"
            <*> o .:? "outputs_per_tx"
            <*> o .:  "fee"
            <*> o .:? "optimistic_recycle"

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
      in case Api.deserialiseFromRawBytesHex @(Api.SigningKey Api.PaymentKey) (BS8.pack hex) of
        Left err ->
          error $ "createSigningKeyAndAddress: Failed to deserialise: " ++ show err
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

{-- TODO: Construct a minimal protocol parameters, see Tx.hs last line.
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

-- | Parse CLI args, load all configuration files, create protocol,
-- generate a signing key, load initial funds, and set up tracers.
--
-- Returns a fully resolved 'Runtime.Runtime' (validated config, rate
-- limiters, pipeline queues, and initial funds already partitioned
-- across workloads).
loadConfig
  :: IO ( -- | Fully resolved runtime (config + rate limiters + queues).
          Runtime.Runtime Fund.Fund tx
          -- | Codec config for serialising blocks on the wire.
        , CodecConfig CardanoBlock
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
  -- Validate config and resolve into a Runtime.
  -- Pipeline: Raw → Validated (with pre-loaded funds) → Runtime.
  validated <- either die pure $ Validated.validate raw funds
  runtime <- Runtime.resolve validated

  -- Load node configuration and create consensus protocol.
  hPutStrLn stderr $ "Loading node config from: " ++ nodeConfigPath
  nodeConfig <- mkNodeConfig nodeConfigPath >>= either die pure
  protocol   <- mkConsensusProtocol nodeConfig >>= either die pure
  let codecConfig  = protocolToCodecConfig protocol
      networkId    = protocolToNetworkId protocol
      networkMagic = protocolToNetworkMagic protocol

  -- Tracers.
  tracers <- Tracing.setupTracers configFile

  pure ( runtime, codecConfig, networkId, networkMagic, tracers )

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
          , shelleyKESFile       = Just ""
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

protocolToCodecConfig :: SomeConsensusProtocol -> CodecConfig CardanoBlock
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
