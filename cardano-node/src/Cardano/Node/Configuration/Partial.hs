{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Cardano.Node.Configuration.Partial
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
    , RequireNetworkMagic (..)
    , NodeProtocol (..)
    , finaliseCardanoConfiguration
    ) where

import           Cardano.Prelude

import           Data.Monoid.Generic

import           Cardano.Node.Configuration.Types

-- | Partial @CardanoConfiguration@ configuration.
data PartialCardanoConfiguration = PartialCardanoConfiguration
    { pccLogPath             :: !(Last FilePath)
    , pccLogConfig           :: !(Last FilePath)
    , pccDBPath              :: !(Last FilePath)
    , pccSocketPath          :: !(Last FilePath)
    , pccApplicationLockFile :: !(Last FilePath)
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
    , pcoRequiresNetworkMagic       :: !(Last RequireNetworkMagic)
    , pcoPBftSigThd                 :: !(Last Double)
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup PartialCore
    deriving Monoid    via GenericMonoid PartialCore

--- | Top-level Cardano SL node configuration
data PartialNode = PartialNode
    { pnoSlotLength                     :: !(Last Integer)
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


-- | Converting a @Last@ to an @Either@
lastToEither :: Text -> Last a -> Either Text a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

--
-- The finalise* family of functions are supposed to be called at the very last stage
-- in the partial options monoid approach, after all the parametrisation layers have been merged,
-- and we're intending to use the resultant config -- they ensure that all values are defined.
--
-- NOTE: we should look into applying generic programming and/or TH for this boilerlate.
--
finaliseCardanoConfiguration :: PartialCardanoConfiguration -> Either Text CardanoConfiguration
finaliseCardanoConfiguration PartialCardanoConfiguration{..} = do

    ccLogPath                <- lastToEither "Unspecified ccLogPath"    pccLogPath
    ccLogConfig              <- lastToEither "Unspecified ccLogConfig"  pccLogConfig
    ccDBPath                 <- lastToEither "Unspecified ccDBPath"     pccDBPath
    ccSocketPath             <- lastToEither "Unspecified ccSocketPath" pccSocketPath
    ccApplicationLockFile    <- lastToEither "Unspecified ccApplicationLockFile"
                                    pccApplicationLockFile

    ccCore                   <- finaliseCore pccCore
    ccNTP                    <- finaliseNTP pccNTP
    ccUpdate                 <- finaliseUpdate pccUpdate
    ccTXP                    <- finaliseTXP pccTXP
    ccDLG                    <- finaliseDLG pccDLG
    ccBlock                  <- finaliseBlock pccBlock
    ccNode                   <- finaliseNode pccNode
    ccTLS                    <- finaliseTLS pccTLS
    ccWallet                 <- finaliseWallet pccWallet

    pure CardanoConfiguration{..}
  where
    -- | Finalize the @PartialCore@, convert to @Core@.
    finaliseCore :: PartialCore -> Either Text Core
    finaliseCore PartialCore{..} = do

        coGenesisFile                   <- lastToEither "Unspecified coGenesisFile"
                                            pcoGenesisFile

        coGenesisHash                   <- lastToEither "Unspecified coGenesisHash"
                                            pcoGenesisHash

        let coNodeId                    = getLast pcoNodeId
        let coNumCoreNodes              = getLast pcoNumCoreNodes

        coNodeProtocol                  <- lastToEither "Unspecified coNodeProtocol"
                                            pcoNodeProtocol

        let coStaticKeySigningKeyFile   = getLast pcoStaticKeySigningKeyFile
        let coStaticKeyDlgCertFile      = getLast pcoStaticKeyDlgCertFile

        coRequiresNetworkMagic          <- lastToEither "Unspecified coRequiresNetworkMagic"
                                            pcoRequiresNetworkMagic

        let coPBftSigThd                = getLast pcoPBftSigThd

        pure Core{..}


    -- | Finalize the @PartialTXP@, convert to @TXP@.
    finaliseTXP :: PartialTXP -> Either Text TXP
    finaliseTXP PartialTXP{..} = do

        txpMemPoolLimitTx           <- lastToEither "Unspecified txpMemPoolLimitTx"
                                        ptxpMemPoolLimitTx

        txpAssetLockedSrcAddress    <- lastToEither "Unspecified txpAssetLockedSrcAddress"
                                        ptxpAssetLockedSrcAddress

        pure TXP{..}

    -- | Finalize the @PartialUpdate@, convert to @Update@.
    finaliseUpdate :: PartialUpdate -> Either Text Update
    finaliseUpdate PartialUpdate{..} = do

        upApplicationName          <- lastToEither "Unspecified upApplicationName"      pupApplicationName
        upApplicationVersion       <- lastToEither "Unspecified upApplicationVersion"   pupApplicationVersion
        upLastKnownBlockVersion    <- finaliseLastKnownBlockVersion pupLastKnownBlockVersion

        pure Update{..}
      where
        finaliseLastKnownBlockVersion :: PartialLastKnownBlockVersion -> Either Text LastKnownBlockVersion
        finaliseLastKnownBlockVersion PartialLastKnownBlockVersion{..} = do

            lkbvMajor  <- lastToEither "Unspecified lkbvMajor"     plkbvMajor
            lkbvMinor  <- lastToEither "Unspecified lkbvMinor"     plkbvMinor
            lkbvAlt    <- lastToEither "Unspecified lkbvAlt"       plkbvAlt

            pure LastKnownBlockVersion{..}

    -- | Finalize the @PartialNTP@, convert to @NTP@.
    finaliseNTP :: PartialNTP -> Either Text NTP
    finaliseNTP PartialNTP{..} = do

        ntpResponseTimeout  <- lastToEither "Unspecified ntpResponseTimeout"    pntpResponseTimeout
        ntpPollDelay        <- lastToEither "Unspecified ntpPollDelay"          pntpPollDelay
        ntpServers          <- lastToEither "Unspecified ntpServers"            pntpServers

        pure NTP{..}

    -- | Finalize the @PartialNode@, convert to @Node@.
    finaliseNode :: PartialNode -> Either Text Node
    finaliseNode PartialNode{..} = do

        noSlotLength                    <- lastToEither "Unspecified noSlotLength"
                                            pnoSlotLength

        noNetworkConnectionTimeout      <- lastToEither "Unspecified noNetworkConnectionTimeout"
                                            pnoNetworkConnectionTimeout

        noHandshakeTimeout              <- lastToEither "Unspecified noHandshakeTimeout"
                                            pnoHandshakeTimeout

        pure Node{..}

    -- | Finalize the @PartialDLG@, convert to @DLG@.
    finaliseDLG :: PartialDLG -> Either Text DLG
    finaliseDLG PartialDLG{..} = do

        dlgCacheParam           <- lastToEither "Unspecified dlgCacheParam"             pdlgCacheParam
        dlgMessageCacheTimeout  <- lastToEither "Unspecified dlgMessageCacheTimeout"    pdlgMessageCacheTimeout

        pure DLG{..}


    -- | Finalize the @PartialBlock@, convert to @Block@.
    finaliseBlock :: PartialBlock -> Either Text Block
    finaliseBlock PartialBlock{..} = do

        blNetworkDiameter        <- lastToEither "Unspecified blNetworkDiameter"        pblNetworkDiameter
        blRecoveryHeadersMessage <- lastToEither "Unspecified blRecoveryHeadersMessage" pblRecoveryHeadersMessage
        blStreamWindow           <- lastToEither "Unspecified blStreamWindow"           pblStreamWindow
        blNonCriticalCQBootstrap <- lastToEither "Unspecified blNonCriticalCQBootstrap" pblNonCriticalCQBootstrap
        blNonCriticalCQ          <- lastToEither "Unspecified blNonCriticalCQ"          pblNonCriticalCQ
        blCriticalCQ             <- lastToEither "Unspecified blCriticalCQ"             pblCriticalCQ
        blCriticalCQBootstrap    <- lastToEither "Unspecified blCriticalCQBootstrap"    pblCriticalCQBootstrap
        blCriticalForkThreshold  <- lastToEither "Unspecified blCriticalForkThreshold"  pblCriticalForkThreshold
        blFixedTimeCQ            <- lastToEither "Unspecified blFixedTimeCQ"            pblFixedTimeCQ

        pure Block{..}

    -- | Finalize the @PartialCertificate@, convert to @Certificate@.
    finaliseCertificate :: PartialCertificate -> Either Text Certificate
    finaliseCertificate PartialCertificate{..} = do

        certOrganization    <- lastToEither "Unspecified certOrganization"  pcertOrganization
        certCommonName      <- lastToEither "Unspecified certCommonName"    pcertCommonName
        certExpiryDays      <- lastToEither "Unspecified certExpiryDays"    pcertExpiryDays
        certAltDNS          <- lastToEither "Unspecified certAltDNS"        pcertAltDNS

        pure Certificate{..}

    -- | Finalize the @PartialTLS@, convert to @TLS@.
    finaliseTLS :: PartialTLS -> Either Text TLS
    finaliseTLS PartialTLS{..} = do

        tlsCA       <- finaliseCertificate ptlsCA
        tlsServer   <- finaliseCertificate ptlsServer
        tlsClients  <- finaliseCertificate ptlsClients

        pure TLS{..}

    -- | Finalize the @PartialWallet@, convert to @Wallet@.
    finaliseWallet :: PartialWallet -> Either Text Wallet
    finaliseWallet PartialWallet{..} = do

        thEnabled   <- lastToEither "Unspecified thEnabled" pthEnabled
        thRate      <- lastToEither "Unspecified thRate"    pthRate
        thPeriod    <- lastToEither "Unspecified thPeriod"  pthPeriod
        thBurst     <- lastToEither "Unspecified thBurst"   pthBurst

        pure Wallet {..}
