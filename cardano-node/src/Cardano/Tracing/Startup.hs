{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Tracing.Startup where

import           Prelude

import           Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)

import           Cardano.Slotting.Slot (SlotNo (..))
import           Cardano.Node.Configuration.TopologyP2P (UseLedger (..))
import           Cardano.Node.Configuration.Socket
import           Cardano.Tracing.OrphanInstances.Network ()

import           Cardano.BM.Tracing (HasPrivacyAnnotation (..),
                   HasSeverityAnnotation (..), Severity (..), ToObject (..),
                   TracingVerbosity (..), Transformable (..))
import           Cardano.BM.Data.Tracer (HasTextFormatter (..), mkObject,
                   trStructuredText)

import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (BlockNodeToClientVersion, BlockNodeToNodeVersion)

import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (LocalAddress (..),
                   LocalSocket, NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (DiffusionMode (..),
                   NodeToNodeVersion)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)

import qualified Network.Socket as Socket


data StartupTrace blk =
  -- | Log startup information.
  --
    StartupInfo
      [SocketOrSocketInfo Socket.SockAddr Socket.SockAddr]
      -- ^ node-to-node addresses
      (Maybe (SocketOrSocketInfo LocalSocket LocalAddress))
      -- ^ node-to-client socket path
      (Map NodeToNodeVersion (BlockNodeToNodeVersion blk))
      -- ^ supported node-to-node versions
      (Map NodeToClientVersion (BlockNodeToClientVersion blk))
      -- ^ supported node-to-client versions

  -- | Log peer-to-peer diffusion mode
  | StartupP2PInfo DiffusionMode
    
  | StartupTime UTCTime

  | StartupNetworkMagic NetworkMagic

  | StartupSocketConfigError SocketConfigError

  | StartupDBValidation

  -- | Log that the network configuration is being updated.
  --
  | NetworkConfigUpdate

  -- | Log network configuration update error.
  --
  | NetworkConfigUpdateError Text

  -- | Log peer-to-peer network configuration, either on startup or when its
  -- updated.
  --
  | NetworkConfig [(Int, Map RelayAccessPoint PeerAdvertise)]
                  [RelayAccessPoint]
                  UseLedgerAfter

  -- | Warn when 'EnableP2P' is set.
  | P2PWarning

  -- | Warn that peer-to-peer requires
  -- 'TestEnableDevelopmentNetworkProtocols' to be set.
  --
  | P2PWarningDevelopementNetworkProtocols

  -- | Warn when 'TestEnableDevelopmentNetworkProtocols' is set.
  --
  | WarningDevelopmentNetworkProtocols [NodeToNodeVersion] [NodeToClientVersion]


instance HasSeverityAnnotation (StartupTrace blk) where
    getSeverityAnnotation (StartupSocketConfigError _) = Error
    getSeverityAnnotation (NetworkConfigUpdateError _) = Error
    getSeverityAnnotation P2PWarning = Warning
    getSeverityAnnotation P2PWarningDevelopementNetworkProtocols = Warning
    getSeverityAnnotation WarningDevelopmentNetworkProtocols {} = Warning
    getSeverityAnnotation _ = Info
instance HasPrivacyAnnotation  (StartupTrace blk) where
instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
        => Transformable Text IO (StartupTrace blk) where
    trTransformer = trStructuredText
instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
        => HasTextFormatter (StartupTrace blk) where
    formatText a _ = ppStartupInfoTrace a
instance ( Show (BlockNodeToNodeVersion blk)
         , Show (BlockNodeToClientVersion blk)
         )
        => ToObject (StartupTrace blk) where
    toObject verb (StartupInfo addresses
                                localSocket
                                supportedNodeToNodeVersions
                                supportedNodeToClientVersions)
      = mkObject $
          [ "nodeAddresses" .= Aeson.toJSON (map ppN2NSocketInfo addresses)
          , "localSocket" .= case localSocket of
                Nothing -> Aeson.Null
                Just a  -> String (Text.pack . ppN2CSocketInfo $ a)
          ]
          ++
          case verb of
            MaximalVerbosity ->
              [ "nodeToNodeVersions" .=
                  Aeson.toJSON (map show . Map.assocs $ supportedNodeToNodeVersions)
              , "nodeToClientVersions" .=
                  Aeson.toJSON (map show . Map.assocs $ supportedNodeToClientVersions)
              ]
            _ ->
              [ "maxNodeToNodeVersion" .=
                  case Map.maxViewWithKey supportedNodeToNodeVersions of
                    Nothing     -> String "no-supported-version"
                    Just (v, _) -> String (Text.pack . show $ v)
              , "maxNodeToClientVersion" .=
                  case Map.maxViewWithKey supportedNodeToClientVersions of
                    Nothing     -> String "no-supported-version"
                    Just (v, _) -> String (Text.pack . show $ v)
              ]
    toObject _verb (StartupP2PInfo diffusionMode)
      = mkObject [ "diffusionMode" .= String (Text.pack . show $ diffusionMode) ]
    toObject _verb (StartupTime time) =
      mkObject [ "startupTime" .= String ( Text.pack
                                         . show
                                         . (ceiling :: POSIXTime -> Int)
                                         . utcTimeToPOSIXSeconds
                                         $ time
                                         )
               ]
    toObject _verb (StartupNetworkMagic networkMagic) =
      mkObject [ "networkMagic" .= String ( Text.pack . show . unNetworkMagic
                                          $ networkMagic) ]
    toObject _verb (StartupSocketConfigError err) =
      mkObject [ "error" .= String (Text.pack . show $ err) ]
    toObject _verb StartupDBValidation =
      mkObject [ "message" .= String "start db validation" ]
    toObject _verb NetworkConfigUpdate =
      mkObject [ "message" .= String "ntework configuration update" ]
    toObject _verb (NetworkConfigUpdateError err) =
      mkObject [ "error" .= String err ]
    toObject _verb (NetworkConfig localRoots publicRoots useLedgerAfter) =
      mkObject [ "localRoots" .= Aeson.toJSON localRoots
               , "publicRoots" .= Aeson.toJSON publicRoots
               , "useLedgerAfter" .= UseLedger useLedgerAfter
               ]
    toObject _verb P2PWarning = 
      mkObject [ "message" .= String p2pWarningMessage ]
    toObject _verb P2PWarningDevelopementNetworkProtocols =
      mkObject [ "message" .= String p2pWarningDevelopmentNetworkProtocolsMessage ]
    toObject _ver (WarningDevelopmentNetworkProtocols ntnVersions ntcVersions) =
      mkObject [ "message" .= String "enabled development network protocols"
               , "nodeToNodeDevelopmentVersions" .= String (Text.pack . show $ ntnVersions)
               , "nodeToClientDevelopmentVersions" .= String (Text.pack . show $ ntcVersions)
               ]



-- | Pretty print 'StartupInfoTrace'
--
ppStartupInfoTrace :: ( Show (BlockNodeToNodeVersion blk)
                      , Show (BlockNodeToClientVersion blk)
                      )
                   => StartupTrace blk
                   -> Text
ppStartupInfoTrace (StartupInfo addresses
                                localSocket
                                supportedNodeToNodeVersions
                                supportedNodeToClientVersions)
  = Text.pack
  $ "\n" ++ intercalate "\n"
    [ "node addresses:          " ++ intercalate ", " (map ppN2NSocketInfo addresses)
    , "local socket:            " ++ maybe "NONE" ppN2CSocketInfo localSocket
    , "node-to-node versions:\n"
       ++ intercalate "\n"
          (map (\(v, bv) -> show v ++ "\t" ++ show bv)
        . Map.assocs
        $ supportedNodeToNodeVersions)
    , "node-to-client versions:\n"
       ++ intercalate "\n"
          (map (\(v, bv) -> show v ++ "\t" ++ show bv)
        . Map.assocs
        $ supportedNodeToClientVersions)
    ]

ppStartupInfoTrace (StartupP2PInfo diffusionMode) =
        case diffusionMode of
          InitiatorAndResponderDiffusionMode -> "initiator and responder diffusion mode"
          InitiatorOnlyDiffusionMode         -> "initaitor only diffusion mode"

ppStartupInfoTrace (StartupTime time) =
    Text.pack $ "startup time: "
           ++ ( show
              . (ceiling :: POSIXTime -> Int)
              . utcTimeToPOSIXSeconds
              $ time
              )
ppStartupInfoTrace (StartupNetworkMagic networkMagic) =
    "network magic: " <> Text.pack (show $ unNetworkMagic networkMagic)

ppStartupInfoTrace (StartupSocketConfigError err) =
    Text.pack $ renderSocketConfigError err

ppStartupInfoTrace StartupDBValidation = "Performing DB validation"

ppStartupInfoTrace NetworkConfigUpdate = "Performing topology configuration update"
ppStartupInfoTrace (NetworkConfigUpdateError err) = err
ppStartupInfoTrace (NetworkConfig localRoots publicRoots useLedgerAfter) =
    Text.pack
  $ intercalate "\n"
  [ "\nLocal Root Groups:"
  , "  " ++ intercalate "\n  " (map (\(x,y) -> show (x, Map.assocs y))
                                    localRoots)
  , "Public Roots:"
  , "  " ++ intercalate "\n  " (map show publicRoots)
  , case useLedgerAfter of
      UseLedgerAfter slotNo -> "Get root peers from the ledger after slot "
                            ++ show (unSlotNo slotNo)
      DontUseLedger         -> "Don't use ledger to get root peers."
  ]

ppStartupInfoTrace P2PWarning = p2pWarningMessage

ppStartupInfoTrace P2PWarningDevelopementNetworkProtocols =
    p2pWarningDevelopmentNetworkProtocolsMessage

ppStartupInfoTrace (WarningDevelopmentNetworkProtocols ntnVersions ntcVersions) =
     "enabled development network protocols: "
  <> (Text.pack . show $ ntnVersions)
  <> " "
  <> (Text.pack . show $ ntcVersions)


p2pWarningMessage :: Text
p2pWarningMessage =
      "unsupported and unverified version of "
   <> "`cardano-node` with peer-to-peer networking capabilities"

p2pWarningDevelopmentNetworkProtocolsMessage :: Text
p2pWarningDevelopmentNetworkProtocolsMessage =
    "peer-to-peer requires TestEnableDevelopmentNetworkProtocols to be set to True"

--
-- Utils
--

-- | Pretty print 'SocketOrSocketInfo'.
--
ppSocketInfo :: Show sock
             => (info -> String)
             -> SocketOrSocketInfo sock info -> String
ppSocketInfo  ppInfo (SocketInfo addr)   = ppInfo addr
ppSocketInfo _ppInfo (ActualSocket sock) = show sock

ppN2CSocketInfo :: SocketOrSocketInfo LocalSocket LocalAddress
                -> String
ppN2CSocketInfo = ppSocketInfo getFilePath

ppN2NSocketInfo :: SocketOrSocketInfo Socket.SockAddr Socket.SockAddr
                -> String
ppN2NSocketInfo = ppSocketInfo show
