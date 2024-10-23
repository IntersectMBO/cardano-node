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
import qualified PlutusTx.Builtins.HasOpaque as PlutusTx
import qualified PlutusTx.Maybe as PlutusTx
import qualified PlutusLedgerApi.V3.Contexts as V3
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))
import PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))

{-# INLINABLE typedValidator #-}
typedValidator :: V3.ScriptContext -> Bool
typedValidator scriptContext = 
    PlutusTx.isJust mSupplementalDatum
  where
    -- supplementalDatumHash = cardano-cli latest transaction hash-script-data --script-data-value 1
    supplementalDatumHash = V3.DatumHash (PlutusTx.stringToBuiltinByteString "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25")
    txInfo = V3.scriptContextTxInfo scriptContext
    mSupplementalDatum = V3.findDatum supplementalDatumHash txInfo

untypedValidator :: BuiltinData -> BuiltinUnit 
untypedValidator ctx = 
  PlutusTx.check (typedValidator (PlutusTx.unsafeFromBuiltinData ctx) )

supplementalDatumBs :: SBS.ShortByteString
supplementalDatumBs = V3.serialiseCompiledCode $$(PlutusTx.compile [|| untypedValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised supplementalDatumBs
