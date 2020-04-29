{-# LANGUAGE OverloadedStrings #-}

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
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Shelley.Spec.Ledger.Coin (Coin(..))
import           Shelley.Spec.Ledger.Keys (DiscKeyHash (..), GenKeyHash, KeyHash)
import           Shelley.Spec.Ledger.TxData
                   (Addr (..), Credential (..), StakeReference(..))

import           Cardano.Config.Shelley.Genesis


exampleShelleyGenesis :: ShelleyGenesis TPraosStandardCrypto
exampleShelleyGenesis =
  ShelleyGenesis
    { sgStartTime = SystemStart . posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer)
    , sgNetworkMagic = NetworkMagic 4036000900
    , sgProtocolMagicId = ProtocolMagicId 838299499
    , sgActiveSlotsCoeff = 6.259
    , sgDecentralisationParam = 1.9e-2
    , sgSecurityParam = SecurityParam 120842
    , sgEpochLength = EpochSize 1215
    , sgSlotsPerKESPeriod = 8541
    , sgMaxKESEvolutions = 28899
    , sgSlotLength =  slotLengthFromSec 8
    , sgUpdateQuorum = 16991
    , sgMaxMajorPV = 25446
    , sgMaxLovelaceSupply = 71
    , sgMaxBodySize = 239857
    , sgMaxHeaderSize = 217569
    , sgGenDelegs = Map.fromList [(genesisVerKeyHash, delegVerKeyHash)]
    , sgInitialFunds = Map.fromList [(initialFundedAddress,initialFunds)]
    }
 where
  -- hash of the genesis verification key
  genesisVerKeyHash :: GenKeyHash TPraosStandardCrypto
  genesisVerKeyHash = DiscKeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb19452b4ed96"
  -- hash of the delegators verififation key
  delegVerKeyHash :: KeyHash TPraosStandardCrypto
  delegVerKeyHash = DiscKeyHash "839b047f56e50654bdb504832186dc1ee0c73c8de2daec7ae62738273be825b2"
  initialFundedAddress :: Addr TPraosStandardCrypto
  initialFundedAddress = Addr paymentCredential (StakeRefBase stakingCredential)
    where
      paymentCredential =
        KeyHashObj $ DiscKeyHash
          "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5a2e89798"
      stakingCredential =
        KeyHashObj $ DiscKeyHash
          "e37a65ea2f9bcefb645de4312cf13d8ac12ae61cf242a9aa2973c9ee32e99ce2"

  initialFunds :: Coin
  initialFunds = Coin 12157196
