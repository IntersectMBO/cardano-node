{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
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

module Cardano.Benchmarking.PlutusScripts.SupplementalDatum (script) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV3, PlutusScriptVersion (..),
                   Script (..), toScriptInAnyLang)

import           Cardano.Benchmarking.ScriptAPI
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))
import           Prelude as Haskell (String, (.), (<$>))
import           PlutusTx
import qualified Data.ByteString.Short as SBS
import qualified PlutusLedgerApi.V1.Scripts as V1
import qualified PlutusLedgerApi.V3 as V3
import qualified PlutusTx.AssocMap as PlutusTx
import qualified PlutusTx.Builtins as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))

-- | Write to disk with: cabal run plutus-scripts-bench -- print SupplementalDatum -o supplemental-datum.plutus
{-# INLINABLE typedValidator #-}
typedValidator :: V3.ScriptContext -> Bool
typedValidator scriptContext = 
    PlutusTx.elem supplementalDatum datums
  where
    txInfo = V3.scriptContextTxInfo scriptContext
    datumMap = V3.txInfoData txInfo
    datums = PlutusTx.elems datumMap

{-# INLINABLE supplementalDatum #-}
supplementalDatum :: V1.Datum 
supplementalDatum = V1.Datum (PlutusTx.mkI 1)

untypedValidator :: BuiltinData -> BuiltinUnit 
untypedValidator ctx = 
  PlutusTx.check (typedValidator (PlutusTx.unsafeFromBuiltinData ctx) )

supplementalDatumBs :: SBS.ShortByteString
supplementalDatumBs = V3.serialiseCompiledCode $$(PlutusTx.compile [|| untypedValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised supplementalDatumBs
