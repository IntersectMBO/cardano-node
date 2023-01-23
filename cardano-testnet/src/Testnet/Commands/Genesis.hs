{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | All Byron and Shelley Genesis related functionality
module Testnet.Commands.Genesis
  ( defaultByronGenesisJsonValue
  , createShelleyGenesisInitialTxIn
  ) where

import           Prelude

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson

import           Hedgehog.Internal.Property

import           Testnet.Util.Process


-- | We need a Byron genesis in order to be able to hardfork to the later Shelley based eras.
-- The values here don't matter as the testnet conditions are ultimately determined
-- by the Shelley genesis.
defaultByronGenesisJsonValue :: Value
defaultByronGenesisJsonValue =
  object
    [ "heavyDelThd" .= toJSON @String "300000000000"
    , "maxBlockSize" .= toJSON @String "2000000"
    , "maxTxSize" .= toJSON @String "4096"
    , "maxHeaderSize" .= toJSON @String "2000000"
    , "maxProposalSize" .= toJSON @String "700"
    , "mpcThd" .= toJSON @String "20000000000000"
    , "scriptVersion" .= toJSON @Int 0
    , "slotDuration" .= toJSON @String "1000"
    , "softforkRule" .= object
      [ "initThd" .= toJSON @String "900000000000000"
      , "minThd" .= toJSON @String "600000000000000"
      , "thdDecrement" .= toJSON @String "50000000000000"
      ]
    , "txFeePolicy" .= object
      [ "multiplier" .= toJSON @String "43946000000"
      , "summand" .= toJSON @String "155381000000000"
      ]
    , "unlockStakeEpoch" .= toJSON @String "18446744073709551615"
    , "updateImplicit" .= toJSON @String "10000"
    , "updateProposalThd" .= toJSON @String "100000000000000"
    , "updateVoteThd" .= toJSON @String "1000000000000"
    ]

-- | The Shelley initial UTxO is constructed from the 'sgInitialFunds' field which
-- is not a full UTxO but just a map from addresses to coin values. Therefore this
-- command creates a transaction input that defaults to the 0th index and therefore
-- we can spend spend this tx input in a transaction.
createShelleyGenesisInitialTxIn
  :: (MonadTest m, MonadCatch m, MonadIO m)
  => Int -> FilePath -> m String
createShelleyGenesisInitialTxIn testnetMagic vKeyFp =
  execCli
      [ "genesis", "initial-txin"
      , "--testnet-magic", show @Int testnetMagic
      , "--verification-key-file", vKeyFp
      ]
