{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Configuration.Lib
    ( finaliseCardanoConfiguration
    ) where

import           Cardano.Prelude

import           Cardano.Node.Configuration.PartialTypes (PartialBlock (..), PartialCardanoConfiguration (..),
                                                          PartialCertificate (..),
                                                          PartialCore (..),
                                                          PartialDLG (..),
                                                          PartialLastKnownBlockVersion (..),
                                                          PartialNTP (..),
                                                          PartialNode (..),
                                                          PartialTLS (..),
                                                          PartialTXP (..),
                                                          PartialUpdate (..),
                                                          PartialWallet (..))
import           Cardano.Node.Configuration.Types (Block (..),
                                                   CardanoConfiguration (..),
                                                   Certificate (..), Core (..),
                                                   DLG (..),
                                                   LastKnownBlockVersion (..),
                                                   NTP (..), Node (..),
                                                   TLS (..), TXP (..),
                                                   Update (..), Wallet (..))

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

        noSystemStartTime               <- lastToEither "Unspecified noSystemStartTime"
                                            pnoSystemStartTime

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

