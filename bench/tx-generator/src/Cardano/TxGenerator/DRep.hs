{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides means to work with DReps as part of tx generation workflows
--   To secure a fund, the key locking the transaction ouput in genesis has to be provided.
module Cardano.TxGenerator.DRep
       ( module Cardano.TxGenerator.DRep
       ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley (Hash (DRepKeyHash))


data DRep era = DRep
  { drepSKey    :: !(SigningKey DRepKey)
  , drepCert    :: !(Certificate era)
  }
  deriving Show

createDRep :: forall era .
     ConwayEraOnwards era
  -> Lovelace
  -> IO (DRep era)
createDRep w deposit = do
  drepSKey <- generateSigningKey AsDRepKey
  let
    DRepKeyHash drepKeyHash = verificationKeyHash $ getVerificationKey drepSKey
    drepCred = Ledger.KeyHashObj $ conwayEraOnwardsConstraints w drepKeyHash

    reqs :: DRepRegistrationRequirements era
    reqs = DRepRegistrationRequirements w drepCred deposit

    drepCert = makeDrepRegistrationCertificate reqs Nothing
  pure DRep{..}


-- from cardano-cli: can be removed once TODO has been resolved
-- TODO: Conway era - Add constrctor for SigningKey DrepKey to ShelleyWitnessSigningKey
castDrep :: SigningKey DRepKey -> SigningKey PaymentKey
castDrep (DRepSigningKey sk) = PaymentSigningKey sk
