{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Api.InMode.Export
  ( applyTxErrorToJson
  ) where

import           Cardano.Api.Orphans ()
import           Cardano.Api.InMode.ToJson ()
import           Cardano.Prelude
import           Data.Aeson (ToJSON(..), Value(..))

import qualified Cardano.Ledger.AuxiliaryData as Core
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Bbody as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

applyTxErrorToJson ::
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.AuxiliaryDataHash (Ledger.Crypto era))
  , ToJSON (Core.TxOut era)
  , ToJSON (Core.Value era)
  , ToJSON (Ledger.PredicateFailure (Core.EraRule "DELEGS" era))
  , ToJSON (Ledger.PredicateFailure (Core.EraRule "PPUP" era))
  , ToJSON (Ledger.PredicateFailure (Core.EraRule "UTXO" era))
  , ToJSON (Ledger.PredicateFailure (Core.EraRule "UTXOW" era))
  ) => Consensus.ApplyTxErr (Consensus.ShelleyBlock era) -> Value
applyTxErrorToJson (Consensus.ApplyTxError predicateFailures) = toJSON (fmap toJSON predicateFailures)
