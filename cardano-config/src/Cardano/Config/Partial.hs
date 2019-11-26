{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.Partial
    ( PartialCardanoConfiguration (..)
    , PartialCore (..)
    , PartialNode (..)
    , PartialNTP (..)
    , PartialUpdate (..)
    , PartialLastKnownBlockVersion (..)
    , PartialTXP (..)
    , PartialDLG (..)
    , PartialBlock (..)
    , PartialTLS (..)
    , PartialCertificate (..)
    , PartialWallet (..)
    -- * re-exports
    , NodeProtocol (..)
    , mkCardanoConfiguration
    ) where

import           Prelude (String)
import           Cardano.Prelude

import           Data.Monoid.Generic

import qualified Ouroboros.Consensus.BlockchainTime as Consensus

import           Cardano.Config.Types
import           Cardano.Config.Topology
import           Cardano.Crypto (RequiresNetworkMagic)

-- | Partial @CardanoConfiguration@ configuration.
data PartialCardanoConfiguration = PartialCardanoConfiguration
    { pccLogPath             :: !(Last FilePath)
    , pccLogConfig           :: !(Last FilePath)
    , pccDBPath              :: !(Last FilePath)
    , pccSocketDir           :: !(Last FilePath)
    , pccApplicationLockFile :: !(Last FilePath)
    , pccTopologyInfo        :: !(Last TopologyInfo)
    , pccNodeAddress         :: !(Last NodeAddress)
    , pccProtocol            :: !(Last Protocol)
    , pccViewMode            :: !(Last ViewMode)
    , pccLogMetrics          :: !(Last Bool)
    , pccTraceOptions        :: !(Last TraceOptions)
    , pccCore                :: !PartialCore
    , pccNTP                 :: !PartialNTP
    , pccUpdate              :: !PartialUpdate
    , pccTXP                 :: !PartialTXP
    , pccDLG                 :: !PartialDLG
    , pccBlock               :: !PartialBlock
    , pccNode                :: !PartialNode
    , pccTLS                 :: !PartialTLS
    , pccWallet              :: !PartialWallet
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialCardanoConfiguration
    deriving Monoid    via GenericMonoid PartialCardanoConfiguration

-- | Partial @Core@ configuration.
data PartialCore = PartialCore
    { pcoGenesisFile                :: !(Last FilePath)
    , pcoGenesisHash                :: !(Last Text)
    , pcoNodeId                     :: !(Last Int)
    -- ^ Core node ID, the number of the node.
    , pcoNumCoreNodes               :: !(Last Int)
    -- ^ The number of the core nodes.
    , pcoNodeProtocol               :: !(Last NodeProtocol)
    -- ^ The type of protocol run on the node.
    , pcoStaticKeySigningKeyFile    :: !(Last FilePath)
    , pcoStaticKeyDlgCertFile       :: !(Last FilePath)
    , pcoRequiresNetworkMagic       :: !(Last RequiresNetworkMagic)
    , pcoPBftSigThd                 :: !(Last Double)
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialCore
    deriving Monoid    via GenericMonoid PartialCore

--- | Top-level Cardano SL node configuration
data PartialNode = PartialNode
    { pnoSlotLength                     :: !(Last Consensus.SlotLength)
    -- ^ Slot length time.
    , pnoNetworkConnectionTimeout       :: !(Last Int)
    -- ^ Network connection timeout in milliseconds.
    , pnoHandshakeTimeout               :: !(Last Int)
    -- ^ Protocol acknowledgement timeout in milliseconds.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialNode
    deriving Monoid    via GenericMonoid PartialNode

-- | Partial @NTP@ configuration.
data PartialNTP = PartialNTP
    { pntpResponseTimeout :: !(Last Int)
    -- ^ NTP response timeout.
    , pntpPollDelay       :: !(Last Int)
    -- ^ NTP poll delay.
    , pntpServers         :: !(Last [Text])
    -- ^ A list of NTP servers.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialNTP
    deriving Monoid    via GenericMonoid PartialNTP

-- | Partial @TXP@ configuration.
data PartialTXP = PartialTXP
    { ptxpMemPoolLimitTx        :: !(Last Int)
    -- ^ Limit on the number of transactions that can be stored in the mem pool.
    , ptxpAssetLockedSrcAddress :: !(Last [Text])
    -- ^ Set of source address which are asset-locked. Transactions which
    -- use these addresses as transaction inputs will be silently dropped.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialTXP
    deriving Monoid    via GenericMonoid PartialTXP

-- | Partial @Update@ configuration.
data PartialUpdate = PartialUpdate
    { pupApplicationName       :: !(Last Text)
    -- ^ Update application name.
    , pupApplicationVersion    :: !(Last Int)
    -- ^ Update application version.
    , pupLastKnownBlockVersion :: !PartialLastKnownBlockVersion
    -- ^ Update last known block version.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialUpdate
    deriving Monoid    via GenericMonoid PartialUpdate

-- | Partial @LastKnownBlockVersion@ configuration.
data PartialLastKnownBlockVersion = PartialLastKnownBlockVersion
    { plkbvMajor :: !(Last Int)
    -- ^ Last known block version major.
    , plkbvMinor :: !(Last Int)
    -- ^ Last known block version minor.
    , plkbvAlt   :: !(Last Int)
    -- ^ Last known block version alternative.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialLastKnownBlockVersion
    deriving Monoid    via GenericMonoid PartialLastKnownBlockVersion

-- | Partial @DLG@ configuration.
data PartialDLG = PartialDLG
    { pdlgCacheParam          :: !(Last Int)
      -- ^ This value parameterizes size of cache used in Delegation.
      -- Not bytes, but number of elements.
    , pdlgMessageCacheTimeout :: !(Last Int)
      -- ^ Interval we ignore cached messages for if it's sent again.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialDLG
    deriving Monoid    via GenericMonoid PartialDLG

-- | Partial @Block@ configuration.
data PartialBlock = PartialBlock
    { pblNetworkDiameter        :: !(Last Int)
      -- ^Estimated time needed to broadcast message from one node to all other nodes.
    , pblRecoveryHeadersMessage :: !(Last Int)
      -- ^Maximum amount of headers node can put into headers message while in "after offline" or "recovery" mode.
    , pblStreamWindow           :: !(Last Int)
      -- ^ Number of blocks to have inflight
    , pblNonCriticalCQBootstrap :: !(Last Double)
      -- ^ If chain quality in bootstrap era is less than this value, non critical misbehavior will be reported.
    , pblNonCriticalCQ          :: !(Last Double)
      -- ^ If chain quality after bootstrap era is less than this value, non critical misbehavior will be reported.
    , pblCriticalCQ             :: !(Last Double)
      -- ^ If chain quality after bootstrap era is less than this value, critical misbehavior will be reported.
    , pblCriticalCQBootstrap    :: !(Last Double)
      -- ^ If chain quality in bootstrap era is less than this value, critical misbehavior will be reported.
    , pblCriticalForkThreshold  :: !(Last Int)
      -- ^ Number of blocks such that if so many blocks are rolled back, it requires immediate reaction.
    , pblFixedTimeCQ            :: !(Last Int)
      -- ^ Chain quality will be also calculated for this amount of seconds.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialBlock
    deriving Monoid    via GenericMonoid PartialBlock

-- | Partial @TLS@ configuration.
data PartialTLS = PartialTLS
    { ptlsCA      :: !PartialCertificate
    -- ^ Certificate Authoritiy certificate.
    , ptlsServer  :: !PartialCertificate
    -- ^ Server certificate.
    , ptlsClients :: !PartialCertificate
    -- ^ Client certificate.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialTLS
    deriving Monoid    via GenericMonoid PartialTLS

-- | Partial @Certificate@ configuration.
data PartialCertificate = PartialCertificate
    { pcertOrganization :: !(Last Text)
    -- ^ Certificate organization.
    , pcertCommonName   :: !(Last Text)
    -- ^ Certificate common name.
    , pcertExpiryDays   :: !(Last Int)
    -- ^ Certificate days of expiration.
    , pcertAltDNS       :: !(Last [Text])
    -- ^ Certificate alternative DNS.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialCertificate
    deriving Monoid    via GenericMonoid PartialCertificate

-- | Partial @Wallet@ configuration.
data PartialWallet = PartialWallet
    { pthEnabled :: !(Last Bool)
    -- ^ Is throttle enabled?
    , pthRate    :: !(Last Int)
    -- ^ Throttle rate.
    , pthPeriod  :: !(Last Text)
    -- ^ Throttle period.
    , pthBurst   :: !(Last Int)
    -- ^ Throttle burst.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialWallet
    deriving Monoid    via GenericMonoid PartialWallet


-- | Return an error if the @Last@ option is incomplete.
mkComplete :: String -> Last a -> Either ConfigError a
mkComplete name (Last x) = maybe (Left $ PartialConfigValue name) Right x

-- This utilizes the mk* family of functions to make sure we have all
-- the required configuration values which then allows us to create a
-- 'CardanoConfiguration' value. This is called at the last stage of the
-- Partial Options Monoid approach.
-- https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
mkCardanoConfiguration :: PartialCardanoConfiguration -> Either ConfigError CardanoConfiguration
mkCardanoConfiguration PartialCardanoConfiguration{..} = do

    ccLogPath                <- mkComplete "ccLogPath"    pccLogPath
    let ccLogConfig          = getLast pccLogConfig
    ccDBPath                 <- mkComplete "ccDBPath"     pccDBPath
    ccSocketDir              <- mkComplete "ccSocketPath" pccSocketDir
    ccApplicationLockFile    <- mkComplete "ccApplicationLockFile"
                                    pccApplicationLockFile

    ccTopologyInfo           <- mkComplete "ccTopologyInfo" pccTopologyInfo
    ccNodeAddress            <- mkComplete "ccNodeAddress" pccNodeAddress
    ccProtocol               <- mkComplete "ccProtocol" pccProtocol
    ccViewMode               <- mkComplete "ccViewMode" pccViewMode
    ccLogMetrics             <- mkComplete "ccLogMetrics" pccLogMetrics
    ccTraceOptions           <- mkComplete "ccTraceOptions" pccTraceOptions
    ccCore                   <- mkCore pccCore
    ccNTP                    <- mkNTP pccNTP
    ccUpdate                 <- mkUpdate pccUpdate
    ccTXP                    <- mkTXP pccTXP
    ccDLG                    <- mkDLG pccDLG
    ccBlock                  <- mkBlock pccBlock
    ccNode                   <- mkNode pccNode
    ccTLS                    <- mkTLS pccTLS
    ccWallet                 <- mkWallet pccWallet

    pure CardanoConfiguration{..}
  where
    -- | Finalize the @PartialCore@, convert to @Core@.
    mkCore :: PartialCore -> Either ConfigError Core
    mkCore PartialCore{..} = do

        coGenesisFile                   <- mkComplete "coGenesisFile"
                                            pcoGenesisFile

        coGenesisHash                   <- mkComplete "coGenesisHash"
                                            pcoGenesisHash

        let coNodeId                    = getLast pcoNodeId
        let coNumCoreNodes              = getLast pcoNumCoreNodes

        coNodeProtocol                  <- mkComplete "coNodeProtocol"
                                            pcoNodeProtocol

        let coStaticKeySigningKeyFile   = getLast pcoStaticKeySigningKeyFile
        let coStaticKeyDlgCertFile      = getLast pcoStaticKeyDlgCertFile

        coRequiresNetworkMagic          <- mkComplete "coRequiresNetworkMagic"
                                            pcoRequiresNetworkMagic

        let coPBftSigThd                = getLast pcoPBftSigThd

        pure Core{..}


    -- | Finalize the @PartialTXP@, convert to @TXP@.
    mkTXP :: PartialTXP -> Either ConfigError TXP
    mkTXP PartialTXP{..} = do

        txpMemPoolLimitTx           <- mkComplete "txpMemPoolLimitTx"
                                        ptxpMemPoolLimitTx

        txpAssetLockedSrcAddress    <- mkComplete "txpAssetLockedSrcAddress"
                                        ptxpAssetLockedSrcAddress

        pure TXP{..}

    -- | Finalize the @PartialUpdate@, convert to @Update@.
    mkUpdate :: PartialUpdate -> Either ConfigError Update
    mkUpdate PartialUpdate{..} = do

        upApplicationName          <- mkComplete "upApplicationName"      pupApplicationName
        upApplicationVersion       <- mkComplete "upApplicationVersion"   pupApplicationVersion
        upLastKnownBlockVersion    <- mkLastKnownBlockVersion pupLastKnownBlockVersion

        pure Update{..}
      where
        mkLastKnownBlockVersion :: PartialLastKnownBlockVersion -> Either ConfigError LastKnownBlockVersion
        mkLastKnownBlockVersion PartialLastKnownBlockVersion{..} = do

            lkbvMajor  <- mkComplete "lkbvMajor"     plkbvMajor
            lkbvMinor  <- mkComplete "lkbvMinor"     plkbvMinor
            lkbvAlt    <- mkComplete "lkbvAlt"       plkbvAlt

            pure LastKnownBlockVersion{..}

    -- | Finalize the @PartialNTP@, convert to @NTP@.
    mkNTP :: PartialNTP -> Either ConfigError NTP
    mkNTP PartialNTP{..} = do

        ntpResponseTimeout  <- mkComplete "ntpResponseTimeout"    pntpResponseTimeout
        ntpPollDelay        <- mkComplete "ntpPollDelay"          pntpPollDelay
        ntpServers          <- mkComplete "ntpServers"            pntpServers

        pure NTP{..}

    -- | Finalize the @PartialNode@, convert to @Node@.
    mkNode :: PartialNode -> Either ConfigError Node
    mkNode PartialNode{..} = do

        noSlotLength                    <- mkComplete "noSlotLength"
                                            pnoSlotLength

        noNetworkConnectionTimeout      <- mkComplete "noNetworkConnectionTimeout"
                                            pnoNetworkConnectionTimeout

        noHandshakeTimeout              <- mkComplete "noHandshakeTimeout"
                                            pnoHandshakeTimeout

        pure Node{..}

    -- | Finalize the @PartialDLG@, convert to @DLG@.
    mkDLG :: PartialDLG -> Either ConfigError DLG
    mkDLG PartialDLG{..} = do

        dlgCacheParam           <- mkComplete "dlgCacheParam"             pdlgCacheParam
        dlgMessageCacheTimeout  <- mkComplete "dlgMessageCacheTimeout"    pdlgMessageCacheTimeout

        pure DLG{..}


    -- | Finalize the @PartialBlock@, convert to @Block@.
    mkBlock :: PartialBlock -> Either ConfigError Block
    mkBlock PartialBlock{..} = do

        blNetworkDiameter        <- mkComplete "blNetworkDiameter"        pblNetworkDiameter
        blRecoveryHeadersMessage <- mkComplete "blRecoveryHeadersMessage" pblRecoveryHeadersMessage
        blStreamWindow           <- mkComplete "blStreamWindow"           pblStreamWindow
        blNonCriticalCQBootstrap <- mkComplete "blNonCriticalCQBootstrap" pblNonCriticalCQBootstrap
        blNonCriticalCQ          <- mkComplete "blNonCriticalCQ"          pblNonCriticalCQ
        blCriticalCQ             <- mkComplete "blCriticalCQ"             pblCriticalCQ
        blCriticalCQBootstrap    <- mkComplete "blCriticalCQBootstrap"    pblCriticalCQBootstrap
        blCriticalForkThreshold  <- mkComplete "blCriticalForkThreshold"  pblCriticalForkThreshold
        blFixedTimeCQ            <- mkComplete "blFixedTimeCQ"            pblFixedTimeCQ

        pure Block{..}

    -- | Finalize the @PartialCertificate@, convert to @Certificate@.
    mkCertificate :: PartialCertificate -> Either ConfigError Certificate
    mkCertificate PartialCertificate{..} = do

        certOrganization    <- mkComplete "certOrganization"  pcertOrganization
        certCommonName      <- mkComplete "certCommonName"    pcertCommonName
        certExpiryDays      <- mkComplete "certExpiryDays"    pcertExpiryDays
        certAltDNS          <- mkComplete "certAltDNS"        pcertAltDNS

        pure Certificate{..}

    -- | Finalize the @PartialTLS@, convert to @TLS@.
    mkTLS :: PartialTLS -> Either ConfigError TLS
    mkTLS PartialTLS{..} = do

        tlsCA       <- mkCertificate ptlsCA
        tlsServer   <- mkCertificate ptlsServer
        tlsClients  <- mkCertificate ptlsClients

        pure TLS{..}

    -- | Finalize the @PartialWallet@, convert to @Wallet@.
    mkWallet :: PartialWallet -> Either ConfigError Wallet
    mkWallet PartialWallet{..} = do

        thEnabled   <- mkComplete "thEnabled" pthEnabled
        thRate      <- mkComplete "thRate"    pthRate
        thPeriod    <- mkComplete "thPeriod"  pthPeriod
        thBurst     <- mkComplete "thBurst"   pthBurst

        pure Wallet {..}
