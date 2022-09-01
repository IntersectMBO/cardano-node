{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Ledger.Constraints
  ( hashAnnotatedConstraints
  ) where

import qualified Cardano.Api.Eras as Eras
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Ledger (ShelleyEraTxBody)

hashAnnotatedConstraints
  :: Eras.ShelleyBasedEra era
  -> (( Ledger.ShelleyEraTxBody (Eras.ShelleyLedgerEra era)
      , Ledger.HashAnnotated (Core.TxBody (Eras.ShelleyLedgerEra era)) Core.EraIndependentTxBody Crypto.StandardCrypto
      ) => a
     )
  -> a
hashAnnotatedConstraints Eras.ShelleyBasedEraShelley f = f
hashAnnotatedConstraints Eras.ShelleyBasedEraAllegra f = f
hashAnnotatedConstraints Eras.ShelleyBasedEraMary    f = f
hashAnnotatedConstraints Eras.ShelleyBasedEraAlonzo  f = f
hashAnnotatedConstraints Eras.ShelleyBasedEraBabbage f = f
hashAnnotatedConstraints Eras.ShelleyBasedEraConway  f = f
