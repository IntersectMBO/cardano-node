{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Cardano.Api.Examples
  ( exampleAll
  , exampleAny
  , exampleMofN
  , exampleShelleyGenesis
  ) where

import           Cardano.Prelude
import           Prelude (error)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Cardano.Api.Typed (MultiSigScript (..))
import qualified Cardano.Api.Typed as Api
import           Cardano.Slotting.Slot (EpochSize (..))
import           Ouroboros.Consensus.Shelley.Node (emptyGenesisStaking)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Consensus.Util.Time

import           Shelley.Spec.Ledger.Address (Addr (..))
import           Shelley.Spec.Ledger.BaseTypes (Network (..), truncateUnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Credential (Credential (..), PaymentCredential,
                     StakeCredential, StakeReference (..))
import           Shelley.Spec.Ledger.Keys (GenDelegPair (..), Hash, KeyHash (..), KeyRole (..),
                     VerKeyVRF)
import           Shelley.Spec.Ledger.PParams (PParams' (..), emptyPParams)

import           Cardano.Api.Shelley.Genesis


exampleAll :: MultiSigScript
exampleAll =
  RequireAllOf [ RequireSignature
                   $ convertToHash "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
               , RequireSignature
                   $ convertToHash "a687dcc24e00dd3caafbeb5e68f97ca8ef269cb6fe971345eb951756"
               , RequireSignature
                   $ convertToHash "0bd1d702b2e6188fe0857a6dc7ffb0675229bab58c86638ffa87ed6d"
               , RequireSignature
                   $ convertToHash "dd0044a26cf7d4491ecea720fda11afb59d5725b53afa605fdf695e6"
               , RequireSignature
                   $ convertToHash "cf223afe150cc8e89f11edaacbbd55b011ba44fbedef66fbd37d8c9d"
               , RequireSignature
                   $ convertToHash "372643e7ef4b41fd2649ada30a89d35cb90b7c14cb5de252e6ce6cb7"
               , RequireSignature
                   $ convertToHash "aa453dc184c5037d60e3fbbadb023f4a41bac112f249b76be9bb37ad"
               , RequireSignature
                   $ convertToHash "6b732c60c267bab894854d6dd57a04a94e603fcc4c36274c9ed75952"
               ]


exampleAny :: MultiSigScript
exampleAny =
  RequireAnyOf [ RequireSignature
                   $ convertToHash "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09"
               , RequireSignature
                   $ convertToHash "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321"
               , RequireSignature
                   $ convertToHash "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8"
               , RequireSignature
                   $ convertToHash "b12e094d1db7c0fba5121f22db193d0060efed8be43654f861bb68ae"
               , RequireSignature
                   $ convertToHash "9be49d56442b4b8b16cab4e43e238bbdefc6c803d554c82fcd5facc3"
               , RequireSignature
                   $ convertToHash "622be5fab3b5c3f371a50a535e4d3349c942a98cecee93b24e2fd11d"
               ]

exampleMofN :: MultiSigScript
exampleMofN =
  RequireMOf 2 [ RequireSignature
                   $ convertToHash "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413"
               , RequireSignature
                   $ convertToHash "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614"
               , RequireSignature
                   $ convertToHash "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538"
               , RequireSignature
                   $ convertToHash "686024aecb5884d73a11b9ae4e63931112ba737e878d74638b78513a"
               ]

convertToHash :: Text -> Api.Hash Api.PaymentKey
convertToHash txt =
  case Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey) $ Text.encodeUtf8 txt of
    Just payKeyHash -> payKeyHash
    Nothing -> error $ "Test.Cardano.Api.Examples.convertToHash: Error deserialising payment key hash: "
                     <> Text.unpack txt

exampleShelleyGenesis :: ShelleyGenesis TPraosStandardCrypto
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer)
    , sgNetworkMagic = 4036000900
    , sgNetworkId = Testnet
    , sgActiveSlotsCoeff = 6.259
    , sgSecurityParam = 120842
    , sgEpochLength = EpochSize 1215
    , sgSlotsPerKESPeriod = 8541
    , sgMaxKESEvolutions = 28899
    , sgSlotLength =  secondsToNominalDiffTime 8
    , sgUpdateQuorum = 16991
    , sgMaxLovelaceSupply = 71
    , sgProtocolParams = emptyPParams
        { _d = truncateUnitInterval (fromRational 1.9e-2)
        , _maxBBSize = 239857
        , _maxBHSize = 217569
        }
    , sgGenDelegs = Map.fromList
                      [( genesisVerKeyHash
                       , GenDelegPair delegVerKeyHash delegVrfKeyHash)
                      ]
    , sgInitialFunds = Map.fromList [(initialFundedAddress,initialFunds)]
    , sgStaking = emptyGenesisStaking
    }
 where
  -- hash of the genesis verification key
  genesisVerKeyHash :: KeyHash Genesis TPraosStandardCrypto
  genesisVerKeyHash = KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
  -- hash of the delegators verififation key
  delegVerKeyHash :: KeyHash GenesisDelegate TPraosStandardCrypto
  delegVerKeyHash = KeyHash "839b047f56e50654bdb504832186dc1ee0c73c8de2daec7ae6273827"
  delegVrfKeyHash :: Hash TPraosStandardCrypto (VerKeyVRF TPraosStandardCrypto)
  delegVrfKeyHash = "231391e7ec1c450a8518134cf6fad1a8e0ed7ffd66d740f8e8271347a6de7bf2"
  initialFundedAddress :: Addr TPraosStandardCrypto
  initialFundedAddress = Addr Testnet paymentCredential (StakeRefBase stakingCredential)
    where
      paymentCredential :: PaymentCredential TPraosStandardCrypto
      paymentCredential =
        KeyHashObj $ KeyHash
          "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"

      stakingCredential :: StakeCredential TPraosStandardCrypto
      stakingCredential =
        KeyHashObj $ KeyHash
          "e37a65ea2f9bcefb645de4312cf13d8ac12ae61cf242a9aa2973c9ee"

  initialFunds :: Coin
  initialFunds = Coin 12157196
