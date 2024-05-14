{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This validator script is based on the Plutus benchmark
--      'Hash n bytestrings onto G2 and add points'
--  cf. https://github.com/IntersectMBO/plutus/blob/master/plutus-benchmark/bls12-381-costs/test/9.6/bls12-381-costs.golden

module Cardano.Benchmarking.PlutusScripts.HashOntoG2AndAdd (script) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV3, PlutusScriptVersion (..),
                   Script (..), toScriptInAnyLang)

import           Cardano.Benchmarking.ScriptAPI
import qualified PlutusLedgerApi.V3 as PlutusV3

import           Prelude as Haskell (String, (.), (<$>))

-- import           Data.ByteString (ByteString)
import qualified Data.ByteString.Short as SBS

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified PlutusTx
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))


scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))


{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum red _txContext =
  case PlutusV3.fromBuiltinData red of
    Nothing -> Tx.traceError "invalid redeemer"
    Just (n, l) ->
      if n < (1000000 :: Integer) -- large number ensures same bitsize for all counter values
      then traceError "redeemer is < 1000000"
      else loop n l
  where
    hashAndAddG2 :: [BuiltinByteString] -> BuiltinBLS12_381_G2_Element
    hashAndAddG2 l =
      go l (Tx.bls12_381_G2_uncompress Tx.bls12_381_G2_compressed_zero)
      where go [] !acc     = acc
            go (q:qs) !acc = go qs $ Tx.bls12_381_G2_add (Tx.bls12_381_G2_hashToGroup q emptyByteString) acc
    loop i l
      | i == 1000000 = ()
      | otherwise    = let !_ = hashAndAddG2 l in loop (pred i) l

hashAndAddG2ShortBs :: SBS.ShortByteString
hashAndAddG2ShortBs = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised hashAndAddG2ShortBs
