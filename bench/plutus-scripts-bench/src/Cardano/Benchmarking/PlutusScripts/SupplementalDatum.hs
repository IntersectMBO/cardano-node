{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Benchmarking.PlutusScripts.SupplementalDatum (script) where

import           Cardano.Api (PlutusScriptVersion (PlutusScriptV3))
import           Cardano.Benchmarking.ScriptAPI
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL), Loc (loc_module), qLocation)
import qualified PlutusLedgerApi.V3 as V3
import qualified PlutusLedgerApi.V3.Contexts as V3
import qualified PlutusTx (compile)
import qualified PlutusTx.Builtins as PlutusTx
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))
import qualified PlutusTx.Prelude as PlutusTx
import           Prelude as Haskell ((.), (<$>))


script :: PlutusBenchScript
script = mkPlutusBenchScriptFromCompiled
           PlutusScriptV3
           $(LitE . StringL . loc_module <$> qLocation)
           $$(PlutusTx.compile [|| untypedValidator ||])


-- | Write to disk with: cabal run plutus-scripts-bench -- print SupplementalDatum -o supplemental-datum.plutus
{-# INLINABLE typedValidator #-}
typedValidator :: V3.ScriptContext -> Bool
typedValidator scriptContext =
    PlutusTx.isJust (V3.findDatum supplementalDatumHash txInfo)
  where
    txInfo = V3.scriptContextTxInfo scriptContext

{- On chain we are dealing with raw bytes. You can generate the datum hash from the cli via:
> cardano-cli transaction hash-script-data --script-data-value 1
> ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25
However this result is hex encoded. Therefore we have to convert to decimal notation and then use `integerToByteString` to convert to bytes.
 -}
{-# INLINABLE supplementalDatumHash  #-}
supplementalDatumHash :: V3.DatumHash
supplementalDatumHash = V3.DatumHash $ PlutusTx.integerToByteString PlutusTx.BigEndian 0 107688188478553082748947992068553556338831975613033640413719911361848497815077

{-# INLINABLE untypedValidator  #-}
untypedValidator :: BuiltinData -> BuiltinUnit
untypedValidator ctx =
  PlutusTx.check (typedValidator (PlutusTx.unsafeFromBuiltinData ctx) )

