{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
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
import qualified GHC.Base
import           Prelude (error, unlines)

#ifdef UNIX
import qualified Control.Concurrent.Async as Async
#endif
import           Control.Tracer
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, breakOn, pack, take)
import           Network.HostName (getHostName)
import           Network.Socket (AddrInfo)
import           System.Directory (canonicalizePath, makeAbsolute)

import           Control.Monad.Class.MonadSTM

#ifdef UNIX
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (..))
#endif
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                     TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace
import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Config.Types (MiscellaneousFilepaths(..),
                                       NodeConfiguration (..), ViewMode (..))

import           Ouroboros.Network.Block
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Node (NodeKernel (getChainDB),
                     ConnectionId (..), DiffusionTracers (..), DiffusionArguments (..),
                     DnsSubscriptionTarget (..), IPSubscriptionTarget (..),
                     RunNode (nodeNetworkMagic, nodeStartTime), IsProducer (..))
import qualified Ouroboros.Consensus.Node as Node (run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.STM (onEachChange)

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.ImmutableDB (ValidationPolicy (..))
import           Ouroboros.Storage.VolatileDB (BlockValidationPolicy (..))

import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol (SomeProtocol(..), fromProtocol)
import           Cardano.Config.Topology
import           Cardano.Config.Types (DbFile(..), NodeAddress(..),
                                       NodeMockCLI(..), NodeProtocolMode (..),
                                       NodeCLI(..), parseNodeConfiguration)
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

    (mscFp', genHash') <- return $ extractFilePathsAndGenHash npm

    nc <- parseNodeConfiguration npm
    traceWith tracer $ "tracing verbosity = " ++
                         case traceVerbosity $ ncTraceOptions nc of
                           NormalVerbosity -> "normal"
                           MinimalVerbosity -> "minimal"
                           MaximalVerbosity -> "maximal"
    eitherSomeProtocol <- runExceptT $ fromProtocol
                                         genHash'
                                         (ncNodeId nc)
                                         (ncNumCoreNodes nc)
                                         (genesisFile mscFp')
                                         (ncReqNetworkMagic nc)
                                         (ncPbftSignatureThresh nc)
                                         (delegCertFile mscFp')
                                         (signKeyFile mscFp')
                                         (ncUpdate nc)
                                         (ncProtocol nc)

    SomeProtocol (p :: Consensus.Protocol blk) <-
      case eitherSomeProtocol of
        Left err -> (putTextLn . pack $ show err) >> exitFailure
        Right (SomeProtocol p) -> pure $ SomeProtocol p

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
  => Consensus.Protocol blk
  -> Trace IO Text
  -> Tracers ConnectionId blk
  -> NodeProtocolMode
  -> (NodeKernel IO ConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode p trace nodeTracers npm onKernel = do

  let pInfo@ProtocolInfo{ pInfoConfig = cfg } = protocolInfo p
      tracer = contramap pack $ toLogObject trace

  -- Node configuration
  nc <- parseNodeConfiguration npm

  createTracers npm tracer cfg

  addrs <- nodeAddressInfo npm

  dbPath <- canonDbPath npm

  eitherTopology <- readTopologyFile npm

  nt <- either (\err -> panic $ "Cardano.Node.Run.readTopologyFile: " <> err) pure eitherTopology

  myLocalAddr <- nodeLocalSocketAddrInfo npm

  let diffusionArguments :: DiffusionArguments
      diffusionArguments = createDiffusionArguments addrs myLocalAddr ipProducers dnsProducers
      diffusionTracers :: DiffusionTracers
      diffusionTracers = createDiffusionTracers nodeTracers
      dnsProducers :: [DnsSubscriptionTarget]
      dnsProducers = dnsSubscriptionTarget <$> dnsProducerAddrs
      ipProducers :: IPSubscriptionTarget
      ipProducers = ipSubscriptionTargets ipProducerAddrs
      (dnsProducerAddrs, ipProducerAddrs) = producerAddresses nt

  removeStaleLocalSocket npm

  varTip <- atomically $ newTVar GenesisPoint

  Node.run
    (consensusTracers nodeTracers)
    (protocolTracers nodeTracers)
    (withTip varTip $ chainDBTracer nodeTracers)
    diffusionTracers
    diffusionArguments
    (nodeNetworkMagic (Proxy @blk) cfg)
    dbPath
    pInfo
    (isProducer nc)
    (customiseChainDbArgs $ dbValidation npm)
    identity -- No NodeParams customisation
    $ \registry nodeKernel -> do
      -- Watch the tip of the chain and store it in @varTip@ so we can include
      -- it in trace messages.
      let chainDB = getChainDB nodeKernel
      void $ onEachChange registry identity Nothing
                          (ChainDB.getTipPoint chainDB) $ \tip ->
        atomically $ writeTVar varTip tip
      onKernel nodeKernel
 where
  customiseChainDbArgs :: Bool
                       -> ChainDB.ChainDbArgs IO blk
                       -> ChainDB.ChainDbArgs IO blk
  customiseChainDbArgs runValid args
    | runValid
    = args
      { ChainDB.cdbImmValidation = ValidateAllEpochs
      , ChainDB.cdbVolValidation = ValidateAll
      }
    | otherwise
    = args

  isProducer :: NodeConfiguration -> IsProducer
  isProducer nc = case p of
   -- For the real protocol, look at the leader credentials
   Consensus.ProtocolRealPBFT _ _ _ _ (Just _) -> IsProducer
   Consensus.ProtocolRealPBFT _ _ _ _ Nothing -> IsNotProducer
   -- For mock protocols, look at the NodeId
   _ -> case ncNodeId nc of
          Just (CoreId _) -> IsProducer
          _               -> IsNotProducer

  createDiffusionTracers :: Tracers ConnectionId blk -> DiffusionTracers
  createDiffusionTracers nodeTracers' = DiffusionTracers
    { dtIpSubscriptionTracer = ipSubscriptionTracer nodeTracers'
    , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers'
    , dtDnsResolverTracer = dnsResolverTracer nodeTracers'
    , dtErrorPolicyTracer = errorPolicyTracer nodeTracers'
    , dtMuxTracer = muxTracer nodeTracers'
    , dtMuxLocalTracer = nullTracer
    , dtHandshakeTracer = nullTracer
    , dtHandshakeLocalTracer = nullTracer
    }

  createTracers
    :: NodeProtocolMode
    -> Tracer IO GHC.Base.String
    -> Consensus.NodeConfig (BlockProtocol blk)
    -> IO ()
  createTracers npm' tracer cfg = do
     case npm' of
       RealProtocolMode (NodeCLI _ _ rNodeAddr _ runDBValidation) -> do
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
           , "Host node address: " <> show rNodeAddr
           , "My DNS producers are " <> show dnsProducerAddrs
           , "My IP producers are " <> show ipProducerAddrs
           , "**************************************"
           ]

         when runDBValidation $ traceWith tracer "Performing DB validation"

       MockProtocolMode (NodeMockCLI _ _ mockNodeAddress _ runDBValidation) -> do
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
                                         <> show mockNodeAddress
                                         <> ", Node Id "
                                         <> show nodeid
                                         <> " not found in topology"

         traceWith tracer $ unlines
                               [ ""
                               , "**************************************"
                               , "I am Node "        <> show mockNodeAddress <> " Id: " <> show nodeid
                               , "My producers are " <> show producers'
                               , "**************************************"
                               ]

         when runDBValidation $ traceWith tracer "Performing DB validation"


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeProtocolMode -> IO FilePath
canonDbPath npm  = do
  dbFp <- case npm of
            MockProtocolMode (NodeMockCLI mscFp' _ _ _ _) -> do
              nodeid <- nid npm
              pure $ (unDB $ dBFile mscFp') <> "-" <> show nodeid

            RealProtocolMode (NodeCLI mscFp' _ _ _ _) -> pure . unDB $ dBFile mscFp'

  canonicalizePath =<< makeAbsolute dbFp

dbValidation :: NodeProtocolMode -> Bool
dbValidation (MockProtocolMode (NodeMockCLI _ _ _ _ dbval)) = dbval
dbValidation (RealProtocolMode (NodeCLI _ _ _ _ dbval)) = dbval

createDiffusionArguments
  :: [AddrInfo]
  -> AddrInfo
  -> IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> DiffusionArguments
createDiffusionArguments addrs myLocalAddr ipProducers dnsProducers =
  DiffusionArguments
    { daAddresses = addrs
    , daLocalAddress = myLocalAddr
    , daIpProducers = ipProducers
    , daDnsProducers = dnsProducers
    }

dnsSubscriptionTarget :: RemoteAddress -> DnsSubscriptionTarget
dnsSubscriptionTarget ra =
  DnsSubscriptionTarget { dstDomain  = BSC.pack (raAddress ra)
                        , dstPort    = raPort ra
                        , dstValency = raValency ra
                        }

extractFilePathsAndGenHash :: NodeProtocolMode -> (MiscellaneousFilepaths, Text)
extractFilePathsAndGenHash npm =
  case npm of
    MockProtocolMode (NodeMockCLI mMscFp genHash _ _ _) ->
      (mMscFp, genHash)
    RealProtocolMode (NodeCLI rMscFp genHash _ _ _) ->
      (rMscFp, genHash)

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
