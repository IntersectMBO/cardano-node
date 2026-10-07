{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-
Module      : Cardano.TxGenerator.Utils
Description : Utility functions used across the transaction generator.
-}
module  Cardano.TxGenerator.Utils
        (module Cardano.TxGenerator.Utils)
        where

import           Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Parser.Text as P

import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.Types

import           GHC.Stack


-- | `keyAddress` determines an address for the relevant era.
keyAddress :: forall era. IsShelleyBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeShelleyAddressInEra
      (shelleyBasedEra @era)
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

-- TODO: check sufficient funds and minimumValuePerUtxo
inputsToOutputsWithFee :: L.Coin -> Int -> [L.Coin] -> [L.Coin]
inputsToOutputsWithFee fee count inputs = map (quantityToLovelace . Quantity) outputs
  where
    (Quantity totalAvailable) = lovelaceToQuantity $ sum inputs - fee
    (out, rest) = divMod totalAvailable (fromIntegral count)
    outputs = (out + rest) : replicate (count-1) out

-- | 'includeChange' gets use made of it as a value splitter in
-- 'Cardano.TxGenerator.Tx.sourceToStoreTransactionNew' by
-- 'Cardano.Benchmarking.Script.Core.evalGenerator'.
includeChange :: L.Coin -> [L.Coin] -> [L.Coin] -> PayWithChange
includeChange fee spend have = case compare changeValue 0 of
  GT -> PayWithChange changeValue spend
  EQ -> PayExact spend
  LT -> error $ "includeChange: Bad transaction: insufficient funds" ++
                "\n   have: " ++ show have ++
                "\n  spend: " ++ show spend ++
                "\n    fee: " ++ show fee
  where changeValue = sum have - sum spend - fee


-- some convenience constructors

-- | `toAnyTxInWitness` converts an old-API witness for spending a transaction
-- input into the experimental API's `Exp.AnyWitness`, decoding a Plutus script
-- if there is one.
-- `Exp.legacyWitnessConversion` only looks at the `Exp.WitTxIn` constructor (to
-- pick the spending purpose) and not at the `TxIn` it carries, so a placeholder
-- is used: one witness is shared by all the funds paid to a script.
toAnyTxInWitness :: forall era. Exp.EraCommonConstraints era
  => Witness WitCtxTxIn era
  -> Either TxGenError (Exp.AnyWitness (ShelleyLedgerEra era))
toAnyTxInWitness witness =
  case Exp.legacyWitnessConversion (convert $ Exp.useEra @era) [(Exp.WitTxIn placeholderTxIn, BuildTxWith witness)] of
    Right [(_, anyWitness)] -> Right anyWitness
    Right _                 -> Left $ TxGenError "toAnyTxInWitness: expected exactly one converted witness"
    Left err                -> Left $ TxGenError $ "toAnyTxInWitness: " ++ show err
 where
  placeholderTxIn = mkTxIn "0000000000000000000000000000000000000000000000000000000000000000#0"

-- | `mkTxInModeCardano` never uses the `TxInByronSpecial` constructor
-- because its type enforces it being a Shelley-based era.
mkTxInModeCardano :: IsShelleyBasedEra era => Tx era -> TxInMode
mkTxInModeCardano = TxInMode shelleyBasedEra

-- | Convert text representation of a txin "hash#txid" to a TxIn e.g. "dbaff4e270cfb55612d9e2ac4658a27c79da4a5271c6f90853042d1403733810#0"
-- Partial. Useful in tests.
mkTxIn :: HasCallStack => Text -> TxIn
mkTxIn = either error id . P.runParser parseTxIn
