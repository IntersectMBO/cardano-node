{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Testnet.PlutusExample.PlutusVersion2.RequireRedeemer
  ( requireRedeemerScript
  , requireRedeemerScriptShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

import qualified PlutusLedgerApi.V2 as Plutus
import           PlutusLedgerApi.V2.Contexts as V2

import           Prelude hiding (($), (&&))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import           Plutus.Script.Utils.Typed as Scripts
import qualified PlutusTx
import           PlutusTx.Builtins
import           PlutusTx.Eq as PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))
import qualified PlutusTx.Prelude as PlutusPrelude

-- serialiseData is a PlutusV2 builtin

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> V2.ScriptContext -> Bool
mkValidator _ redeemer sc =
  serialiseData redeemer PlutusTx./= emptyByteString &&
  PlutusPrelude.isJust (PlutusPrelude.find
    (PlutusTx.== Plutus.OutputDatum (Plutus.Datum $ PlutusTx.toBuiltinData (42 :: Integer)))
    txinsDatums) &&
  PlutusPrelude.isJust (PlutusPrelude.find
    (PlutusTx.== Plutus.OutputDatum (Plutus.Datum $ PlutusTx.toBuiltinData (42 :: Integer)))
    referenceInputDatums)
 where
  txInfo = V2.scriptContextTxInfo sc
  txinsDatums = PlutusPrelude.map (txOutDatum . txInInfoResolved)
                  $ V2.txInfoInputs txInfo
  referenceInputDatums =
    PlutusPrelude.map (txOutDatum . txInInfoResolved)
      $ V2.txInfoReferenceInputs txInfo

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.mkUntypedValidator mkValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

requireRedeemerScriptShortBs :: SBS.ShortByteString
requireRedeemerScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

requireRedeemerScript :: PlutusScript PlutusScriptV2
requireRedeemerScript = PlutusScriptSerialised requireRedeemerScriptShortBs
