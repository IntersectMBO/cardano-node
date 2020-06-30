{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.Tests
  ( keyTests
  , certificateTests
  , txTests
  ) where

import           Cardano.Prelude

import           Test.CLI.Shelley.TextEnvelope.Golden.Certificates.StakeAddressCertificates
                   (golden_shelleyStakeAddressCertificates)
import           Test.CLI.Shelley.TextEnvelope.Golden.Certificates.MIRCertificate
                   (golden_shelleyMIRCertificate)
import           Test.CLI.Shelley.TextEnvelope.Golden.Certificates.OperationalCertificate
                   (golden_shelleyOperationalCertificate)
import           Test.CLI.Shelley.TextEnvelope.Golden.Certificates.StakePoolCertificates
                   (golden_shelleyStakePoolCertificates)

import           Test.CLI.Shelley.TextEnvelope.Golden.Keys.GenesisDelegateKeys
                   (golden_shelleyGenesisDelegateKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.Keys.GenesisUTxOKeys
                   (golden_shelleyGenesisUTxOKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.Keys.GenesisKeys
                   (golden_shelleyGenesisKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.Keys.KESKeys
                   (golden_shelleyKESKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.Keys.PaymentKeys
                   (golden_shelleyPaymentKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.Keys.StakeKeys
                   (golden_shelleyStakeKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.Keys.VRFKeys
                   (golden_shelleyVRFKeys)

import           Test.CLI.Shelley.TextEnvelope.Golden.Tx.Tx (golden_shelleyTx)
import           Test.CLI.Shelley.TextEnvelope.Golden.Tx.TxBody (golden_shelleyTxBody)

import qualified Hedgehog as H

keyTests :: IO Bool
keyTests =
  H.checkSequential
    $ H.Group "TextEnvelope Key Goldens"
        [ ("golden_shelleyPaymentKeys", golden_shelleyPaymentKeys)
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
