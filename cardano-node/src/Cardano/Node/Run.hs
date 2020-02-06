{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
import           Prelude (error, id, unlines)

#ifdef UNIX
import qualified Control.Concurrent.Async as Async
#endif
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Tracer
import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LB
import           Data.Either (partitionEithers)
import           Data.Functor.Contravariant (contramap)
import qualified Data.List as List
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text (Text, breakOn, pack, take)
import           Network.HostName (getHostName)
import           System.Directory (canonicalizePath, makeAbsolute)

import           Control.Monad.Class.MonadSTM

#ifdef UNIX
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (..))
#endif
import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..),
                     TracingVerbosity (..), setHostname)
import           Cardano.Config.Logging (LoggingLayer (..))
import           Cardano.Config.Types (MiscellaneousFilepaths(..),
                                       NodeConfiguration (..), ViewMode (..))

import           Ouroboros.Network.Block

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

import           Cardano.Common.LocalSocket
import           Cardano.Config.Protocol (SomeProtocol(..), fromProtocol)
import           Cardano.Config.Topology
import           Cardano.Config.Types (ConfigYamlFilePath(..), DbFile(..), NodeMockCLI(..),
                                       NodeProtocolMode (..), NodeCLI(..),TopologyFile(..),
                                       parseNodeConfiguration)
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

    (mscFp', configFp', genHash') <- return $ extractFilePathsAndGenHash npm

    nc <- parseNodeConfiguration $ unConfigPath configFp'
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
  -> Tracer IO (LogObject Text)
  -> Tracers ConnectionId blk
  -> NodeProtocolMode
  -> (NodeKernel IO ConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode p trace nodeTracers npm onKernel = do
  case npm of
    -- Run a node using a real protocol
    RealProtocolMode (NodeCLI rMscFp _ rNodeAddr config runDBValidation) -> do
      nc <- parseNodeConfiguration $ unConfigPath config
      let pInfo@ProtocolInfo{ pInfoConfig = cfg } = protocolInfo p

      -- Topology
      eitherTopology <- readRealNodeTopology . unTopology $ topFile rMscFp
      topology <- case eitherTopology of
                    --TODO: Convert handleSimpleNode to return `ExceptT`
                    Left err -> panic $ "Cardano.Node.Run.readRealNodeTopology: "
                                      <> err
                    Right top -> pure top

      -- Tracing
      let tracer = contramap pack $ toLogObject trace

      traceWith tracer $
        "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

      traceWith tracer $ unlines
        [ ""
        , "**************************************"
        , "Host node address: " <> show rNodeAddr
        , "My producers are " <> (show $ rProducers topology)
        , "**************************************"
        ]

      -- Socket path
      myLocalAddr <- localSocketAddrInfo $ socketFile rMscFp

      addrs <- nodeAddressInfo rNodeAddr

      let ipProducerAddrs  :: [NodeAddress]
          dnsProducerAddrs :: [RemoteAddress]
          (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
            [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
            | ra <- rProducers topology ]
          ipProducers :: IPSubscriptionTarget
          ipProducers =
            let ips = nodeAddressToSockAddr <$> ipProducerAddrs
            in IPSubscriptionTarget {
                  ispIps     = ips,
                  ispValency = length ips
                }

          dnsProducers :: [DnsSubscriptionTarget]
          dnsProducers = producerSubscription <$> dnsProducerAddrs


          producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
          producerSubscription ra =
            DnsSubscriptionTarget
            { dstDomain  = BSC.pack (raAddress ra)
            , dstPort    = raPort ra
            , dstValency = raValency ra
            }
          diffusionTracers :: DiffusionTracers
          diffusionTracers = DiffusionTracers
            { dtIpSubscriptionTracer  = ipSubscriptionTracer  nodeTracers
            , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers
            , dtDnsResolverTracer     = dnsResolverTracer     nodeTracers
            , dtErrorPolicyTracer     = errorPolicyTracer     nodeTracers
            , dtMuxTracer             = muxTracer             nodeTracers
            , dtMuxLocalTracer        = nullTracer
            , dtHandshakeTracer       = nullTracer
            , dtHandshakeLocalTracer  = nullTracer
            }

          diffusionArguments :: DiffusionArguments
          diffusionArguments = DiffusionArguments
            { daAddresses             = addrs
            , daLocalAddress          = myLocalAddr
            , daIpProducers           = ipProducers
            , daDnsProducers          = dnsProducers
            }

      removeStaleLocalSocket  $ socketFile rMscFp
      dbPath <- canonicalizePath =<< makeAbsolute (unDB $ dBFile rMscFp)
      varTip <- atomically $ newTVar GenesisPoint

      when runDBValidation $
        traceWith tracer "Performing DB validation"

      Node.run
        (consensusTracers nodeTracers)
        (withTip varTip $ chainDBTracer nodeTracers)
        diffusionTracers
        diffusionArguments
        (nodeNetworkMagic (Proxy @blk) cfg)
        dbPath
        pInfo
        (isProducer nc)
        (customiseChainDbArgs runDBValidation)
        id -- No NodeParams customisation
        $ \registry nodeKernel -> do
          -- Watch the tip of the chain and store it in @varTip@ so we can include
          -- it in trace messages.
          let chainDB = getChainDB nodeKernel
          void $ onEachChange registry id Nothing
                              (ChainDB.getTipPoint chainDB) $ \tip ->
            atomically $ writeTVar varTip tip
          onKernel nodeKernel
    MockProtocolMode (NodeMockCLI mMscFp _ mockNodeAddress cfgYaml runDBValidation) -> do
      nc <- parseNodeConfiguration $ unConfigPath cfgYaml
      NetworkTopology nodeSetups <- either error id <$> readTopologyFile (unTopology $ topFile mMscFp)

      let pInfo@ProtocolInfo{ pInfoConfig = cfg } = protocolInfo p

                    -- Tracing
      let tracer = contramap pack $ toLogObject trace
      traceWith tracer $ "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

      let producersList = map (\ns -> (nodeId ns, producers ns)) nodeSetups

      let producers' = case (List.lookup (nid nc) producersList) of
                         Just ps ->  ps
                         Nothing -> error $ "handleSimpleNode: own address "
                                       <> show mockNodeAddress
                                       <> ", Node Id "
                                       <> show (nid nc)
                                       <> " not found in topology"

      ----------------------------------------------

      traceWith tracer $ unlines
        [ "**************************************"
        , "I am Node "        <> show mockNodeAddress <> " Id: " <> show (nid nc)
        , "My producers are " <> show producers'
        , "**************************************"
        ]

      -- Socket path
      myLocalAddr <- localSocketAddrInfo $ socketFile mMscFp

      addrs <- nodeAddressInfo mockNodeAddress
      let ipProducerAddrs  :: [NodeAddress]
          dnsProducerAddrs :: [RemoteAddress]
          (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
            [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
            | ra <- producers' ]
          ipProducers :: IPSubscriptionTarget
          ipProducers =
            let ips = nodeAddressToSockAddr <$> ipProducerAddrs
            in IPSubscriptionTarget {
                  ispIps     = ips,
                  ispValency = length ips
                }
          dnsProducers :: [DnsSubscriptionTarget]
          dnsProducers = producerSubscription <$> dnsProducerAddrs

          producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
          producerSubscription ra =
            DnsSubscriptionTarget
            { dstDomain  = BSC.pack (raAddress ra)
            , dstPort    = raPort ra
            , dstValency = raValency ra
            }
          diffusionTracers :: DiffusionTracers
          diffusionTracers = DiffusionTracers
            { dtIpSubscriptionTracer  = ipSubscriptionTracer  nodeTracers
            , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers
            , dtDnsResolverTracer     = dnsResolverTracer     nodeTracers
            , dtErrorPolicyTracer     = errorPolicyTracer     nodeTracers
            , dtMuxTracer             = muxTracer             nodeTracers
            , dtMuxLocalTracer        = nullTracer
            , dtHandshakeTracer       = nullTracer
            , dtHandshakeLocalTracer  = nullTracer
            }

          diffusionArguments :: DiffusionArguments
          diffusionArguments = DiffusionArguments
            { daAddresses             = addrs
            , daLocalAddress          = myLocalAddr
            , daIpProducers           = ipProducers
            , daDnsProducers          = dnsProducers
            }

      removeStaleLocalSocket $ socketFile mMscFp
      dbPath <- canonicalizePath =<< makeAbsolute (unDB $ dBFile mMscFp)

      varTip <- atomically $ newTVar GenesisPoint
      Node.run
        (consensusTracers nodeTracers)
        (withTip varTip $ chainDBTracer nodeTracers)
        diffusionTracers
        diffusionArguments
        (nodeNetworkMagic (Proxy @blk) cfg)
        (dbPath <> "-" <> show (nid nc))
        pInfo
        (isProducer nc)
        (customiseChainDbArgs runDBValidation)
        id -- No NodeParams customisation
        $ \registry nodeKernel -> do
          -- Watch the tip of the chain and store it in @varTip@ so we can include
          -- it in trace messages.
          let chainDB = getChainDB nodeKernel
          void $ onEachChange registry id Nothing
                              (ChainDB.getTipPoint chainDB) $ \tip ->
            atomically $ writeTVar varTip tip
          onKernel nodeKernel
  where
   nid :: NodeConfiguration -> Word64
   nid nc = case ncNodeId nc of
           Just (CoreId (CoreNodeId n)) -> n
           Just (RelayId _) -> panic $ "Cardano.Node.Run.nid: "
                                     <> "Non-core nodes currently not supported"
           Nothing -> panic $ "Cardano.Node.Run.nid: "
                            <> "Please specify a NodeId in your configuration .yaml file"

   customiseChainDbArgs :: Bool
                        -> ChainDB.ChainDbArgs IO blk
                        -> ChainDB.ChainDbArgs IO blk
   customiseChainDbArgs runValid args = args
     { ChainDB.cdbValidation = if runValid
         then ValidateAllEpochs
         else ValidateMostRecentEpoch
     }
   isProducer :: NodeConfiguration -> IsProducer
   isProducer nc = case p of
    -- For the real protocol, look at the leader credentials
    Consensus.ProtocolRealPBFT _ _ _ _ (Just _) -> IsProducer
    Consensus.ProtocolRealPBFT _ _ _ _ Nothing -> IsNotProducer
    -- For mock protocols, look at the NodeId
    _ -> case ncNodeId nc of
           Just (CoreId _) -> IsProducer
           _               -> IsNotProducer

-- | Read the `RealNodeTopology` configuration from the specified file.
-- While running a real protocol, this gives your node its own address and
-- other remote peers it will attempt to connect to.
readRealNodeTopology :: FilePath -> IO (Either Text RealNodeTopology)
readRealNodeTopology fp = do
  ebs <- Exception.try $ BSC.readFile fp :: IO (Either IOException BSC.ByteString)
  case ebs of
    Left e -> pure $ handler e
    Right bs -> pure . first toS . eitherDecode $ LB.fromStrict bs
 where
   handler :: IOException -> Either Text RealNodeTopology
   handler e =  Left . pack $ show e

extractFilePathsAndGenHash
  :: NodeProtocolMode -> (MiscellaneousFilepaths, ConfigYamlFilePath, Text)
extractFilePathsAndGenHash npm =
  case npm of
    MockProtocolMode (NodeMockCLI mMscFp genHash _ configYaml _) ->
      (mMscFp, configYaml, genHash)
    RealProtocolMode (NodeCLI rMscFp genHash _ configYaml _) ->
      (rMscFp, configYaml, genHash)
