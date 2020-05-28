{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Cardano.Config.Examples
  ( exampleShelleyGenesis
  ) where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..))
import           Cardano.Slotting.Slot (EpochSize (..))
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..), slotLengthFromSec)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Consensus.Shelley.Node (emptyGenesisStaking)
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Shelley.Spec.Ledger.Address (Addr (..))
import           Shelley.Spec.Ledger.BaseTypes (Network (..), truncateUnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin(..))
import           Shelley.Spec.Ledger.Credential (Credential (..), StakeReference(..))
import           Shelley.Spec.Ledger.Keys (KeyHash(..), KeyRole(..))
import           Shelley.Spec.Ledger.PParams (PParams' (..), emptyPParams)

import           Cardano.Config.Shelley.Genesis


exampleShelleyGenesis :: ShelleyGenesis TPraosStandardCrypto
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = SystemStart . posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer)
    , sgNetworkMagic = NetworkMagic 4036000900
    , sgNetworkId = Testnet
    , sgProtocolMagicId = ProtocolMagicId 838299499
    , sgActiveSlotsCoeff = 6.259
    , sgSecurityParam = SecurityParam 120842
    , sgEpochLength = EpochSize 1215
    , sgSlotsPerKESPeriod = 8541
    , sgMaxKESEvolutions = 28899
    , sgSlotLength =  slotLengthFromSec 8
    , sgUpdateQuorum = 16991
    , sgMaxMajorPV = 25446
    , sgMaxLovelaceSupply = 71
    , sgProtocolParams = emptyPParams
        { _d = truncateUnitInterval . realToFrac $ (1.9e-2 :: Double)
        , _maxBBSize = 239857
        , _maxBHSize = 217569
        }
    , sgGenDelegs = Map.fromList [(genesisVerKeyHash, delegVerKeyHash)]
    , sgInitialFunds = Map.fromList [(initialFundedAddress,initialFunds)]
    , sgStaking = emptyGenesisStaking
    }
 where
  -- hash of the genesis verification key
  genesisVerKeyHash :: KeyHash Genesis TPraosStandardCrypto
  genesisVerKeyHash = KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb19452b4ed96"
  -- hash of the delegators verififation key
  delegVerKeyHash :: KeyHash GenesisDelegate TPraosStandardCrypto
  delegVerKeyHash = KeyHash "839b047f56e50654bdb504832186dc1ee0c73c8de2daec7ae62738273be825b2"
  initialFundedAddress :: Addr TPraosStandardCrypto
  initialFundedAddress = Addr Testnet paymentCredential (StakeRefBase stakingCredential)
    where
      paymentCredential =
        KeyHashObj $ KeyHash
          "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5a2e89798"
      stakingCredential =
        KeyHashObj $ KeyHash
          "e37a65ea2f9bcefb645de4312cf13d8ac12ae61cf242a9aa2973c9ee32e99ce2"

  initialFunds :: Coin
  initialFunds = Coin 12157196
