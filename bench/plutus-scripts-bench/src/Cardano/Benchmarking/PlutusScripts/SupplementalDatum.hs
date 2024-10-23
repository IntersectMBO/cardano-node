{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | This validator script is based on the Plutus benchmark
--      'Hash n bytestrings onto G2 and add points'
--  cf. https://github.com/IntersectMBO/plutus/blob/master/plutus-benchmark/bls12-381-costs/test/9.6/bls12-381-costs.golden

module Cardano.Benchmarking.PlutusScripts.SupplementalDatum (script) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV3, PlutusScriptVersion (..),
                   Script (..), toScriptInAnyLang)

import           Cardano.Benchmarking.ScriptAPI
import qualified PlutusLedgerApi.V3 as V3
import           Prelude as Haskell (String, (.), (<$>))

import qualified Data.ByteString.Short as SBS

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified PlutusTx.Builtins as PlutusTx
import qualified PlutusTx.AssocMap as PlutusTx
import qualified PlutusLedgerApi.V1.Scripts as V1
import qualified PlutusTx.Builtins.HasOpaque as PlutusTx
-- import qualified PlutusTx.Maybe as PlutusTx
import qualified PlutusLedgerApi.V3.Contexts as V3
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))
import PlutusTx
-- import qualified PlutusTx.Foldable as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))

{-# INLINABLE typedValidator #-}
typedValidator :: V3.ScriptContext -> Bool
typedValidator scriptContext = 
    PlutusTx.elem datum1 datums &&
    PlutusTx.elem datum0 datums
  where
    -- supplementalDatumHash = cardano-cli latest transaction hash-script-data --script-data-value 1
    -- FAILS TO FIND THIS: supplementalDatumHash = V3.DatumHash (PlutusTx.stringToBuiltinByteString "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25")
    -- FAILS TO FIND THIS: supplementalDatumHash = V3.DatumHash (PlutusTx.stringToBuiltinByteString "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314")
    -- Fails to find either datum. So we need to see how big the datum map is
    -- Return to this once you confirm there are two datums in the datum map
    supplementalDatumHash = datum1Hash
    _mSupplementalDatum = V3.findDatum supplementalDatumHash txInfo
    txInfo = V3.scriptContextTxInfo scriptContext
    datumMap = V3.txInfoData txInfo
    datums = PlutusTx.elems datumMap

{-# INLINABLE datum1 #-}
datum1 :: V1.Datum 
datum1 = V1.Datum (PlutusTx.mkI 1)

{-# INLINABLE datum0 #-}
datum0 :: V1.Datum
datum0 = V1.Datum (PlutusTx.mkI 0)

-- Looking up by hash fails. Not sure why 
{-# INLINABLE datum1Hash #-}
datum1Hash :: V3.DatumHash 
datum1Hash = V3.DatumHash $ PlutusTx.encodeUtf8 $ PlutusTx.stringToBuiltinString "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"

untypedValidator :: BuiltinData -> BuiltinUnit 
untypedValidator ctx = 
  PlutusTx.check (typedValidator (PlutusTx.unsafeFromBuiltinData ctx) )

supplementalDatumBs :: SBS.ShortByteString
supplementalDatumBs = V3.serialiseCompiledCode $$(PlutusTx.compile [|| untypedValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised supplementalDatumBs
