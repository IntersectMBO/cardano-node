{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  , ViewMode(..)
  )
where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace)
import           Prelude (String, error, unlines)

import qualified Control.Concurrent.Async as Async
import           Control.Tracer
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, breakOn, pack, take)
import           Data.Version (showVersion)
import           Network.HostName (getHostName)
import           Network.Socket (AddrInfo)
import           System.Directory (canonicalizePath, makeAbsolute)
import qualified System.IO as IO
import qualified System.IO.Error  as IO
import qualified GHC.IO.Handle.FD as IO (fdToHandle)

import           Paths_cardano_node (version)
#ifdef UNIX
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (..))
#endif
import           Cardano.BM.Data.LogItem (LOContent (..),
                     PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                     TracingVerbosity (..), TracingFormatting (..),
                     severityNotice, trTransformer)
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace

import           Cardano.Config.GitRev (gitRev)
import           Cardano.Config.Logging (LoggingLayer (..), Severity (..))
import           Cardano.Config.Types (MiscellaneousFilepaths(..),
                                       NodeConfiguration (..), ViewMode (..))

import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..))
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Node (NodeKernel,
                     DiffusionTracers (..), DiffusionArguments (..),
                     DnsSubscriptionTarget (..), IPSubscriptionTarget (..),
                     RunNode (nodeNetworkMagic, nodeStartTime),
                     RemoteConnectionId)
import qualified Ouroboros.Consensus.Node as Node (run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Config as Consensus
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Consensus.Storage.VolatileDB (BlockValidationPolicy (..))

import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol
                   (SomeConsensusProtocol(..), mkConsensusProtocol,
                    renderProtocolInstantiationError)
import           Cardano.Config.Topology
import           Cardano.Config.Types
import           Cardano.Tracing.Tracers
#ifdef UNIX
import           Cardano.Node.TUI.LiveView
#endif


runNode
  :: LoggingLayer
  -> NodeProtocolMode
  -> IO ()
runNode loggingLayer npm = do
    hn <- hostname
    let !trace = setHostname hn $
                 llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
    let tracer = contramap pack $ toLogObject trace

    mscFp' <- return $ extractMiscFilePaths npm

    nc <- parseNodeConfiguration npm

    traceWith tracer $ "tracing verbosity = " ++
                         case traceVerbosity $ ncTraceOptions nc of
                           NormalVerbosity -> "normal"
                           MinimalVerbosity -> "minimal"
                           MaximalVerbosity -> "maximal"
    eitherSomeProtocol <- runExceptT $ mkConsensusProtocol nc (Just mscFp')

    SomeConsensusProtocol (p :: Consensus.Protocol blk (BlockProtocol blk)) <-
      case eitherSomeProtocol of
        Left err -> (putTextLn $ renderProtocolInstantiationError err) >> exitFailure
        Right (SomeConsensusProtocol p) -> pure $ SomeConsensusProtocol p

    tracers <- mkTracers (ncTraceOptions nc) trace

    case ncViewMode nc of
      SimpleView -> handleSimpleNode p trace tracers npm (const $ pure ())
      LiveView   -> do
#ifdef UNIX
        let c = llConfiguration loggingLayer
        -- We run 'handleSimpleNode' as usual and run TUI thread as well.
        -- turn off logging to the console, only forward it through a pipe to a central logging process
        CM.setDefaultBackends c [KatipBK, TraceForwarderBK, UserDefinedBK "LiveViewBackend"]

        be :: LiveViewBackend blk Text <- realize c
        let lvbe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
        llAddBackend loggingLayer lvbe (UserDefinedBK "LiveViewBackend")
        setTopology be npm
        captureCounters be trace

        -- User will see a terminal graphics and will be able to interact with it.
        nodeThread <- Async.async $ handleSimpleNode p trace tracers npm
                       (setNodeKernel be)
        setNodeThread be nodeThread

        void $ Async.waitAny [nodeThread]
#else
        handleSimpleNode p trace tracers npm (const $ pure ())
#endif
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.

handleSimpleNode
  :: forall blk. RunNode blk
  => Consensus.Protocol blk (BlockProtocol blk)
  -> Trace IO Text
  -> Tracers RemoteConnectionId LocalConnectionId blk
  -> NodeProtocolMode
  -> (NodeKernel IO RemoteConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode p trace nodeTracers npm onKernel = do

  let pInfo@ProtocolInfo{ pInfoConfig = cfg } = Consensus.protocolInfo p
      tracer = contramap pack $ toLogObject trace

  -- Node configuration
  nc <- parseNodeConfiguration npm

  createTracers npm trace tracer cfg

  addrs <- nodeAddressInfo npm

  dbPath <- canonDbPath npm

  eitherTopology <- readTopologyFile npm

  nt <- either (\err -> panic $ "Cardano.Node.Run.readTopologyFile: " <> err) pure eitherTopology

  myLocalAddr <- nodeLocalSocketAddrInfo nc npm

  let diffusionArguments :: DiffusionArguments
      diffusionArguments = createDiffusionArguments addrs myLocalAddr ipProducers dnsProducers
      diffusionTracers :: DiffusionTracers
      diffusionTracers = createDiffusionTracers nodeTracers
      dnsProducers :: [DnsSubscriptionTarget]
      dnsProducers = dnsSubscriptionTarget <$> dnsProducerAddrs
      ipProducers :: IPSubscriptionTarget
      ipProducers = ipSubscriptionTargets ipProducerAddrs
      (dnsProducerAddrs, ipProducerAddrs) = producerAddresses nt

  removedStaleSocket <- runExceptT $ removeStaleLocalSocket nc npm
  case removedStaleSocket of
    Left err   -> (putTextLn $ show err) >> exitFailure
    Right addr -> return addr

  withShutdownHandler npm trace $
   Node.run
    (consensusTracers nodeTracers)
    (protocolTracers nodeTracers)
    (chainDBTracer nodeTracers)
    diffusionTracers
    diffusionArguments
    (nodeNetworkMagic (Proxy @blk) cfg)
    dbPath
    pInfo
    (customiseChainDbArgs $ dbValidation npm)
    identity -- No NodeParams customisation
    $ \_registry nodeKernel -> onKernel nodeKernel
 where
  customiseChainDbArgs :: Bool
                       -> ChainDB.ChainDbArgs IO blk
                       -> ChainDB.ChainDbArgs IO blk
  customiseChainDbArgs runValid args
    | runValid
    = args
      { ChainDB.cdbImmValidation = ValidateAllChunks
      , ChainDB.cdbVolValidation = ValidateAll
      }
    | otherwise
    = args

  createDiffusionTracers :: Tracers RemoteConnectionId LocalConnectionId blk
                         -> DiffusionTracers
  createDiffusionTracers nodeTracers' = DiffusionTracers
    { dtIpSubscriptionTracer = ipSubscriptionTracer nodeTracers'
    , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers'
    , dtDnsResolverTracer = dnsResolverTracer nodeTracers'
    , dtErrorPolicyTracer = errorPolicyTracer nodeTracers'
    , dtLocalErrorPolicyTracer = localErrorPolicyTracer nodeTracers'
    , dtAcceptPolicyTracer = acceptPolicyTracer nodeTracers'
    , dtMuxTracer = muxTracer nodeTracers'
    , dtMuxLocalTracer = nullTracer
    , dtHandshakeTracer = handshakeTracer nodeTracers'
    , dtHandshakeLocalTracer = localHandshakeTracer nodeTracers'
    }

  createTracers
    :: NodeProtocolMode
    -> Trace IO Text
    -> Tracer IO String
    -> Consensus.TopLevelConfig blk
    -> IO ()
  createTracers npm' tr tracer cfg = do
     case npm' of
       RealProtocolMode NodeCLI{nodeAddr, validateDB} -> do
         eitherTopology <- readTopologyFile npm
         nt <- either
                 (\err -> panic $ "Cardano.Node.Run.readTopologyFile: " <> err)
                 pure
                 eitherTopology

         let (dnsProducerAddrs, ipProducerAddrs) = producerAddresses nt

         traceWith tracer $
           "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

         traceWith tracer $ unlines
           [ ""
           , "**************************************"
           , "Host node address: " <> show nodeAddr
           , "My DNS producers are " <> show dnsProducerAddrs
           , "My IP producers are " <> show ipProducerAddrs
           , "**************************************"
           ]

         meta <- mkLOMeta Notice Public
         let rTr = appendName "release" tr
             vTr = appendName "version" tr
             cTr = appendName "commit"  tr
         traceNamedObject rTr (meta, LogMessage "Byron")
         traceNamedObject vTr (meta, LogMessage . pack . showVersion $ version)
         traceNamedObject cTr (meta, LogMessage gitRev)

         when validateDB $ traceWith tracer "Performing DB validation"

       MockProtocolMode NodeMockCLI{mockNodeAddr, mockValidateDB} -> do
         eitherTopology <- readTopologyFile npm
         nodeid <- nid npm
         (MockNodeTopology nodeSetups) <- either
                                            (\err -> panic $ "Cardano.Node.Run.readTopologyFile: " <> err)
                                            pure
                                            eitherTopology

         traceWith tracer $ "System started at " <> show (nodeStartTime (Proxy @blk) cfg)
         let producersList = map (\ns -> (nodeId ns, producers ns)) nodeSetups
             producers' = case (List.lookup nodeid producersList) of
                            Just ps ->  ps
                            Nothing -> error $ "handleSimpleNode: own address "
                                         <> show mockNodeAddr
                                         <> ", Node Id "
                                         <> show nodeid
                                         <> " not found in topology"

         traceWith tracer $ unlines
                               [ ""
                               , "**************************************"
                               , "I am Node "        <> show mockNodeAddr
                                          <> " Id: " <> show nodeid
                               , "My producers are " <> show producers'
                               , "**************************************"
                               ]

         when mockValidateDB $ traceWith tracer "Performing DB validation"

-- | We provide an optional cross-platform method to politely request shut down.
--
-- The parent process passes us the file descriptor number of the read end of a
-- pipe, via the CLI with @--shutdown-ipc FD@. If the write end gets closed,
-- either deliberatly by the parent process or automatically because the parent
-- process itself terminated, then we initiate a clean shutdown.
--
withShutdownHandler :: NodeProtocolMode -> Trace IO Text -> IO () -> IO ()
withShutdownHandler (RealProtocolMode NodeCLI{shutdownIPC = Just (Fd fd)})
                    trace action =
    Async.race_ (wrapUninterruptableIO waitForEOF) action
  where
    waitForEOF :: IO ()
    waitForEOF = do
      hnd <- IO.fdToHandle fd
      r   <- try $ IO.hGetChar hnd
      case r of
        Left e
          | IO.isEOFError e -> traceWith tracer "received shutdown request"
          | otherwise       -> throwIO e

        Right _  ->
          throwIO $ IO.userError "--shutdown-ipc FD does not expect input"

    tracer :: Tracer IO Text
    tracer = trTransformer TextualRepresentation MaximalVerbosity
                           (severityNotice trace)

withShutdownHandler _ _ action = action

-- | Windows blocking file IO calls like 'hGetChar' are not interruptable by
-- asynchronous exceptions, as used by async 'cancel' (as of base-4.12).
--
-- This wrapper works around that problem by running the blocking IO in a
-- separate thread. If the parent thread receives an async cancel then it
-- will return. Note however that in this circumstance the child thread may
-- continue and remain blocked, leading to a leak of the thread. As such this
-- is only reasonable to use a fixed number of times for the whole process.
--
wrapUninterruptableIO :: IO a -> IO a
wrapUninterruptableIO action = async action >>= wait


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeProtocolMode -> IO FilePath
canonDbPath npm  = do
  dbFp <- case npm of
            MockProtocolMode NodeMockCLI{mockMscFp} -> do
              --TODO: we should eliminate auto-naming here too
              nodeid <- nid npm
              pure $ unDB (dBFile mockMscFp) <> "-" <> show nodeid

            RealProtocolMode NodeCLI{mscFp} -> pure . unDB $ dBFile mscFp

  canonicalizePath =<< makeAbsolute dbFp

dbValidation :: NodeProtocolMode -> Bool
dbValidation (MockProtocolMode NodeMockCLI{mockValidateDB}) = mockValidateDB
dbValidation (RealProtocolMode NodeCLI{validateDB}) = validateDB

createDiffusionArguments
  :: [AddrInfo]
  -> FilePath
  -> IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> DiffusionArguments
createDiffusionArguments addrs myLocalAddr ipProducers dnsProducers =
  DiffusionArguments
    { daAddresses = addrs
    , daLocalAddress = myLocalAddr
    , daIpProducers = ipProducers
    , daDnsProducers = dnsProducers
    -- TODO: these limits are arbitrary at the moment;
    -- issue: https://github.com/input-output-hk/ouroboros-network/issues/1836
    , daAcceptedConnectionsLimit = AcceptedConnectionsLimit {
        acceptedConnectionsHardLimit = 512
      , acceptedConnectionsSoftLimit = 384
      , acceptedConnectionsDelay     = 5
      }
    }

dnsSubscriptionTarget :: RemoteAddress -> DnsSubscriptionTarget
dnsSubscriptionTarget ra =
  DnsSubscriptionTarget { dstDomain  = BSC.pack (raAddress ra)
                        , dstPort    = raPort ra
                        , dstValency = raValency ra
                        }

extractMiscFilePaths :: NodeProtocolMode -> MiscellaneousFilepaths
extractMiscFilePaths npm =
  case npm of
    MockProtocolMode NodeMockCLI{mockMscFp} -> mockMscFp
    RealProtocolMode NodeCLI{mscFp} -> mscFp

ipSubscriptionTargets :: [NodeAddress] -> IPSubscriptionTarget
ipSubscriptionTargets ipProdAddrs =
  let ips = nodeAddressToSockAddr <$> ipProdAddrs
  in IPSubscriptionTarget { ispIps = ips
                          , ispValency = length ips
                          }

-- | NodeIds are only required for mock protocols
nid :: NodeProtocolMode -> IO Word64
nid (RealProtocolMode _) = panic $ "Cardano.Node.Run.nid: "
                                 <> "Real protocols do not require node ids"
nid npm@(MockProtocolMode _) = do
   nc <- parseNodeConfiguration npm
   case ncNodeId nc of
        Just (CoreId (CoreNodeId n)) -> pure n
        Just (RelayId _) -> panic $ "Cardano.Node.Run.nid: "
                                 <> "Non-core nodes currently not supported"
        Nothing -> panic $ "Cardano.Node.Run.nid: "
                         <> "Please specify a NodeId in your configuration .yaml file"

producerAddresses :: NetworkTopology -> ([RemoteAddress], [NodeAddress])
producerAddresses nt =
  case nt of
    RealNodeTopology producers' -> partitionEithers $ map remoteOrNode producers'
    MockNodeTopology nodeSetup ->
      partitionEithers . map remoteOrNode $ concatMap producers nodeSetup
 where
   remoteOrNode :: RemoteAddress -> Either RemoteAddress NodeAddress
   remoteOrNode ra = case remoteAddressToNodeAddress ra of
                       Just na -> Right na
                       Nothing -> Left ra
