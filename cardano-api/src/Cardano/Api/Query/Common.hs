{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Query.Common
  ( CurrentEpochState(..)
  , PoolDistribution(..)
  , ProtocolState(..)
  , SerialisedCurrentEpochState(..)
  , SerialisedPoolDistribution(..)
  , UTxO(..)
  ) where

import           Cardano.Api.EraCast
import           Cardano.Api.Eras
import           Cardano.Api.Modes (ConsensusProtocol)
import           Cardano.Api.TxBody

import           Ouroboros.Network.Block (Serialised (..))

import           Control.Monad
import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Core as Core

-- import qualified Data.Map.Strict as Map

-- import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

-- import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
-- import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
-- import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
-- import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus

-- import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
-- import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)
-- import qualified Ouroboros.Consensus.Cardano.Block as Consensus
-- import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
-- import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
-- import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))

-- import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update

-- import           Cardano.Ledger.Binary
-- import qualified Cardano.Ledger.Binary.Plain as Plain
-- import           Cardano.Ledger.Crypto (Crypto)
-- import qualified Cardano.Ledger.Shelley.API as Shelley
-- import qualified Cardano.Ledger.Shelley.LedgerState as Shelley

newtype CurrentEpochState era = CurrentEpochState (Shelley.EpochState (ShelleyLedgerEra era))

newtype PoolDistribution era = PoolDistribution
  { unPoolDistr :: Shelley.PoolDistr (Core.EraCrypto (ShelleyLedgerEra era))
  }

newtype ProtocolState era
  = ProtocolState (Serialised (Consensus.ChainDepState (ConsensusProtocol era)))

newtype SerialisedCurrentEpochState era
  = SerialisedCurrentEpochState (Serialised (Shelley.EpochState (ShelleyLedgerEra era)))

newtype SerialisedPoolDistribution era
  = SerialisedPoolDistribution (Serialised (Shelley.PoolDistr (Core.EraCrypto (ShelleyLedgerEra era))))

newtype UTxO era = UTxO { unUTxO :: Map TxIn (TxOut CtxUTxO era) }
  deriving (Eq, Show)

instance EraCast UTxO where
  eraCast toEra' (UTxO m) = UTxO <$> forM m (eraCast toEra')

instance forall era. (IsShelleyBasedEra era, FromJSON (TxOut CtxUTxO era))
  => FromJSON (UTxO era) where
    parseJSON = Aeson.withObject "UTxO" $ \hm -> do
      let l = HMS.toList $ KeyMap.toHashMapText hm
      res <- mapM toTxIn l
      pure . UTxO $ Map.fromList res
     where
      toTxIn :: (Text, Aeson.Value) -> Aeson.Parser (TxIn, TxOut CtxUTxO era)
      toTxIn (txinText, txOutVal) = do
        (,) <$> parseJSON (Aeson.String txinText)
            <*> parseJSON txOutVal

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m
  toEncoding (UTxO m) = toEncoding m

data AcquiringFailure
  = AFPointTooOld
  | AFPointNotOnChain
  deriving (Eq, Show)
