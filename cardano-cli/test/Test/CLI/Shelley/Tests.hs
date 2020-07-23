{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.Tests
  ( keyTests
  , certificateTests
  , metaDatatests
  , txTests
  ) where

import           Cardano.Prelude

import           Test.CLI.Shelley.Golden.Address.Build
                   (golden_shelleyAddressBuild)
import           Test.CLI.Shelley.Golden.Address.Info
                   (golden_shelleyAddressInfo)
import           Test.CLI.Shelley.Golden.Address.KeyGen
                   (golden_shelleyAddressKeyGen)
import           Test.CLI.Shelley.Golden.Genesis.Create
                   (golden_shelleyGenesisCreate)
import           Test.CLI.Shelley.Golden.Genesis.InitialTxIn
                   (golden_shelleyGenesisInitialTxIn)
import           Test.CLI.Shelley.Golden.Genesis.KeyGenDelegate
                   (golden_shelleyGenesisKeyGenDelegate)
import           Test.CLI.Shelley.Golden.Genesis.KeyGenGenesis
                   (golden_shelleyGenesisKeyGenGenesis)
import           Test.CLI.Shelley.Golden.Genesis.KeyGenUtxo
                   (golden_shelleyGenesisKeyGenUtxo)
import           Test.CLI.Shelley.Golden.Genesis.KeyHash
                   (golden_shelleyGenesisKeyHash)
import           Test.CLI.Shelley.Golden.TextEnvelope.Certificates.StakeAddressCertificates
                   (golden_shelleyStakeAddressCertificates)
import           Test.CLI.Shelley.Golden.TextEnvelope.Certificates.MIRCertificate
                   (golden_shelleyMIRCertificate)
import           Test.CLI.Shelley.Golden.TextEnvelope.Certificates.OperationalCertificate
                   (golden_shelleyOperationalCertificate)
import           Test.CLI.Shelley.Golden.TextEnvelope.Certificates.StakePoolCertificates
                   (golden_shelleyStakePoolCertificates)

import           Test.CLI.Shelley.Golden.Metadata.StakePoolMetadata
                   (golden_stakePoolMetadataHash)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.ExtendedPaymentKeys
                   (golden_shelleyExtendedPaymentKeys)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.GenesisDelegateKeys
                   (golden_shelleyGenesisDelegateKeys)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.GenesisUTxOKeys
                   (golden_shelleyGenesisUTxOKeys)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.GenesisKeys
                   (golden_shelleyGenesisKeys)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.KESKeys
                   (golden_shelleyKESKeys)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.PaymentKeys
                   (golden_shelleyPaymentKeys)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.StakeKeys
                   (golden_shelleyStakeKeys)
import           Test.CLI.Shelley.Golden.TextEnvelope.Keys.VRFKeys
                   (golden_shelleyVRFKeys)

import           Test.CLI.Shelley.Golden.TextEnvelope.Tx.Tx (golden_shelleyTx)
import           Test.CLI.Shelley.Golden.TextEnvelope.Tx.TxBody (golden_shelleyTxBody)

import qualified Hedgehog as H

keyTests :: IO Bool
keyTests =
  H.checkSequential
    $ H.Group "TextEnvelope Key Goldens"
        [ ("golden_shelleyAddressInfo", golden_shelleyAddressInfo)
        , ("golden_shelleyAddressKeyGen", golden_shelleyAddressKeyGen)
        , ("golden_shelleyExtendedPaymentKeys", golden_shelleyExtendedPaymentKeys)
        , ("golden_shelleyGenesisCreate", golden_shelleyGenesisCreate)
        , ("golden_shelleyGenesisInitialTxIn", golden_shelleyGenesisInitialTxIn)
        , ("golden_shelleyGenesisKeyGenDelegate", golden_shelleyGenesisKeyGenDelegate)
        , ("golden_shelleyGenesisKeyGenGenesis", golden_shelleyGenesisKeyGenGenesis)
        , ("golden_shelleyGenesisKeyGenUtxo", golden_shelleyGenesisKeyGenUtxo)
        , ("golden_shelleyGenesisKeyHash", golden_shelleyGenesisKeyHash)
        , ("golden_shelleyPaymentKeys", golden_shelleyPaymentKeys)
        , ("golden_shelleyAddressBuild", golden_shelleyAddressBuild)
        , ("golden_shelleyStakeKeys", golden_shelleyStakeKeys)
        , ("golden_shelleyGenesisKeys", golden_shelleyGenesisKeys)
        , ("golden_shelleyGenesisDelegateKeys", golden_shelleyGenesisDelegateKeys)
        , ("golden_shelleyGenesisUTxOKeys", golden_shelleyGenesisUTxOKeys)
        , ("golden_shelleyKESKeys", golden_shelleyKESKeys)
        , ("golden_shelleyVRFKeys", golden_shelleyVRFKeys)
        ]

txTests :: IO Bool
txTests =
  H.checkSequential
    $ H.Group "TextEnvelope Tx Goldens"
        [ ("golden_shelleyTxBody", golden_shelleyTxBody)
        , ("golden_shelleyTx", golden_shelleyTx)
        ]

certificateTests :: IO Bool
certificateTests =
  H.checkSequential
    $ H.Group "TextEnvelope Certificate Goldens"
        [ ("golden_shelleyStakeAddressCertificates", golden_shelleyStakeAddressCertificates)
        , ("golden_shelleyOperationalCertificate", golden_shelleyOperationalCertificate)
        , ("golden_shelleyStakePoolCertificates", golden_shelleyStakePoolCertificates)
        , ("golden_shelleyMIRCertificate", golden_shelleyMIRCertificate)
        ]

metaDatatests :: IO Bool
metaDatatests =
  H.checkSequential
    $ H.Group "Metadata Goldens"
        [ ("golden_stakePoolMetadataHash", golden_stakePoolMetadataHash)
        ]
